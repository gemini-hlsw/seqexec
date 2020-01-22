// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.data.Nested
import cats.effect.Sync
import cats.effect.Async
import cats.effect.Timer
import cats.implicits._
import mouse.boolean._
import fs2.Stream
import java.lang.{Double => JDouble}
import java.lang.{Integer => JInt}
import java.lang.{Float => JFloat}
import java.util
import java.util.concurrent.atomic.AtomicInteger
import java.util.{Timer => JTimer}
import java.util.TimerTask
import java.util.concurrent.locks.ReentrantLock

import edu.gemini.epics.acm._
import io.chrisdavenport.log4cats.Logger
import seqexec.model.ObserveStage
import seqexec.model.enum.ApplyCommandResult
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.SeqexecFailure.SeqexecException
import seqexec.server.SeqexecFailure.NullEpicsError
import squants.Time

import scala.math.abs
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration

trait EpicsCommand[F[_]] {
  def post(timeout: FiniteDuration): F[ApplyCommandResult]
  def mark: F[Unit]
}

abstract class EpicsCommandBase[F[_]: Async] extends EpicsCommand[F] {
  protected val cs: Option[CaCommandSender]

  override def post(timeout: FiniteDuration): F[ApplyCommandResult] = setTimeout(timeout) *>
    Async[F].async[ApplyCommandResult] { (f: Either[Throwable, ApplyCommandResult] => Unit) =>
      cs.map { ccs =>
        ccs.postCallback {
          new CaCommandListener {
            override def onSuccess(): Unit = f(ApplyCommandResult.Completed.asRight)
            override def onPause(): Unit = f(ApplyCommandResult.Paused.asRight)
            override def onFailure(cause: Exception): Unit = f(cause.asLeft)
          }
        }
      // It should call f on all execution paths, thanks @tpolecat
      }.void.getOrElse(f(SeqexecFailure.Unexpected("Unable to trigger command.").asLeft))
    }

  override def mark: F[Unit] = Sync[F].delay {
      cs.map(_.mark())
    }.void

  protected def setTimeout(t: FiniteDuration): F[Unit] =
    Sync[F].delay {
      cs.map(_.getApplySender).map(_.setTimeout(t.length, t.unit))
    }.void

}

trait EpicsSystem[T] {

  val className: String
  val CA_CONFIG_FILE: String

  def build[F[_]: Sync](service: CaService, tops: Map[String, String]): F[T]

  // Still using a var, but at least now it's hidden.
  private var instanceInternal = Option.empty[T]

  def instance[F[_]: Logger: Sync](service: CaService, tops: Map[String, String]): F[T] =
    instanceInternal.map(_.pure[F])
      .getOrElse(init[F](service, tops))

  def init[F[_]: Logger: Sync](service: CaService, tops: Map[String, String]): F[T] = {
    MonadError[F, Throwable].catchNonFatal[Unit](
      tops.foldLeft((new XMLBuilder).fromStream(this.getClass.getResourceAsStream(CA_CONFIG_FILE))
        .withCaService(service))((b, a) => b.withTop(a._1, a._2)).buildAll()
    ) *>
    (for {
      r <- build[F](service, tops)
      _ <- Sync[F].delay {
        instanceInternal = r.some
      }
    } yield r
    ).onError {
      case c: Throwable =>
        Logger[F].warn(c)(s"$className: Problem initializing EPICS service: ${c.getMessage}")
    }
  }
}

object EpicsCommandBase {

  def setParameter[F[_]: Sync, T, A](p: Option[CaParameter[T]], v: A, f: A => T): F[Unit] = p.map { ch =>
    Sync[F].delay {
      ch.set(f(v))
    }.adaptError {
      case e => SeqexecFailure.Unexpected(
        s"Unable to set parameter on channel ${ch.channel}. Error was ${e.getMessage}"
      )
    }.void
  }.getOrElse{Sync[F].raiseError(SeqexecFailure.Unexpected("Attempt to write parameter to unconnected channel."))}

  def setParameter[F[_]: Sync, T](p: Option[CaParameter[T]], v: T): F[Unit] = setParameter[F, T, T](p, v, x => x )

}

trait ObserveCommand {
  protected val cs: Option[CaCommandSender]
  protected val os: Option[CaApplySender]

  def post[F[_]: Async](timeout: FiniteDuration): F[ObserveCommandResult] = setTimeout(timeout) *>
    Async[F].async[ObserveCommandResult] { (f: Either[Throwable, ObserveCommandResult] => Unit) =>
      os.map { oos =>
        oos.postCallback {
          new CaCommandListener {
            override def onSuccess(): Unit = f(ObserveCommandResult.Success.asRight)
            override def onPause(): Unit = f(ObserveCommandResult.Paused.asRight)
            override def onFailure(cause: Exception): Unit = cause match {
              case _: CaObserveStopped => f(ObserveCommandResult.Stopped.asRight)
              case _: CaObserveAborted => f(ObserveCommandResult.Aborted.asRight)
              case _                   => f(cause.asLeft)
            }
          }
        }
      }.void.getOrElse(f(SeqexecFailure.Unexpected("Unable to trigger command.").asLeft))
    }

  def mark[F[_]: Sync]: F[Unit] = Sync[F].delay {
    cs.map(_.mark())
  }.void

  protected def setTimeout[F[_]: Sync](t: FiniteDuration): F[Unit] =
    Sync[F].delay {
      os.map(_.setTimeout(t.length, t.unit))
    }.void
}

object EpicsCodex {
  //This code deals with decoding and encoding the EPICS values
  trait EncodeEpicsValue[A, T] {
    def encode(a: A): T
  }

  object EncodeEpicsValue {
    def apply[A, T](f: A => T): EncodeEpicsValue[A, T] = (a: A) => f(a)
    def applyO[A, T](f: PartialFunction[A, T]): EncodeEpicsValue[A, Option[T]] = (a: A) => f.lift(a)
  }

  def encode[A, T](a: A)(implicit e: EncodeEpicsValue[A, T]): T = e.encode(a)

  trait DecodeEpicsValue[T, A] {
    def decode(t: T): A
  }

  object DecodeEpicsValue {
    def apply[T, A](f: T => A): DecodeEpicsValue[T, A] = (t: T) => f(t)
  }

  def decode[T, A](t: T)(implicit e: DecodeEpicsValue[T, A]): A = e.decode(t)

}

object EpicsUtil {

  //`locked` gets a piece of code and runs it protected by `lock`
  private def locked[A](lock: ReentrantLock)(f: => A): A = {
    lock.lock()
    try {
      f
    } finally {
      lock.unlock()
    }
  }

  def waitForValuesF[T, F[_]: Async](attr: CaAttribute[T], vv: Seq[T], timeout: FiniteDuration, name: String): F[T] =
    Async[F].async[T] { (f: Either[Throwable, T] => Unit) =>
      //The task is created with async. So we do whatever we need to do,
      // and then call `f` to signal the completion of the task.

      //`resultGuard` and `lock` are used for synchronization.
      val resultGuard = new AtomicInteger(1)
      val lock = new ReentrantLock()

      // First we verify that the attribute doesn't already have the required value.
      if (!attr.values().isEmpty && vv.contains(attr.value)) {
        f(attr.value.asRight)
      } else {
        // If not, we set a timer for the timeout, and a listener for the EPICS
        // channel. The timer and the listener can both complete the IO. The
        // first one to do it cancels the other.The use of `resultGuard`
        // guarantees that only one of them will complete the IO.
        val timer = new JTimer
        val statusListener = new CaAttributeListener[T] {
          override def onValueChange(newVals: util.List[T]): Unit = {
            if (!newVals.isEmpty && vv.contains(newVals.get(0)) && resultGuard.getAndDecrement() === 1) {
              locked(lock) {
                attr.removeListener(this)
                timer.cancel()
              }
              // This `right` looks a bit confusing because is not related to
              // the `TrySeq`, but to the result of `IO`.
              f(newVals.get(0).asRight)
            }
          }

          override def onValidityChange(newValidity: Boolean): Unit = {}
        }

        locked(lock) {
          if (timeout.toMillis > 0) {
            timer.schedule(new TimerTask {
              override def run(): Unit = if (resultGuard.getAndDecrement() === 1) {
                locked(lock) {
                  attr.removeListener(statusListener)
                }
                f(SeqexecFailure.Timeout(name).asLeft)
              }
            }, timeout.toMillis)
          }
          attr.addListener(statusListener)
        }
      }
    }

  def waitForValueF[T, F[_]: Async](attr: CaAttribute[T], v: T, timeout: FiniteDuration, name: String): F[Unit] =
    waitForValuesF[T, F](attr, List(v), timeout, name).void

  def safeAttribute[F[_]: Sync, A](get: => CaAttribute[A]): F[Option[A]] =
    Sync[F].delay(Option(get.value))

  /** Tries to read a value of type A from a channel
   *  Null results are raised as error and other errors are captured
   */
  def safeAttributeWrapF[F[_]: Sync, A >: Null](channel: String, get: => A): F[A] =
    Sync[F].delay(Option(get)) // Wrap the read on Option to do null check
      .adaptError{ case e => SeqexecException(e)} // if we have e.g CAException wrap it
      .ensure(NullEpicsError(channel))(_.isDefined) // equivalent to a null check
      .map{_.orNull} // orNull lets us typecheck but it will never be used due to the `ensure` call above

  def safeAttributeF[F[_]: Sync, A >: Null](get: => CaAttribute[A]): F[A] =
    safeAttributeWrapF(get.channel, get.value)

  def safeAttributeSListF[F[_]: Sync, A >: Null](get: => CaAttribute[A]): F[List[A]] =
    safeAttributeWrapF(get.channel, get.values.asScala.toList)

  def safeAttributeSDouble[F[_]: Sync, A](get: => CaAttribute[JDouble]): F[Option[Double]] =
    Nested(safeAttribute(get)).map(_.toDouble).value

  def safeAttributeSDoubleF[F[_]: Sync](get: => CaAttribute[JDouble]): F[Double] =
    safeAttributeF(get).map(_.toDouble)

  def safeAttributeSListSDoubleF[F[_]: Sync](get: => CaAttribute[JDouble]): F[List[Double]] =
    Nested(safeAttributeSListF(get)).map(_.toDouble).value

  def safeAttributeSFloat[F[_]: Sync, A](get: => CaAttribute[JFloat]): F[Option[Float]] =
    Nested(safeAttribute(get)).map(_.toFloat).value

  def safeAttributeSFloatF[F[_]: Sync](get: => CaAttribute[JFloat]): F[Float] =
    safeAttributeF(get).map(_.toFloat)

  def safeAttributeSListSFloatF[F[_]: Sync](get: => CaAttribute[JFloat]): F[List[Float]] =
    Nested(safeAttributeSListF(get)).map(_.toFloat).value

  def safeAttributeSInt[F[_]: Sync, A](get: => CaAttribute[JInt]): F[Option[Int]] =
    Nested(safeAttribute(get)).map(_.toInt).value

  def safeAttributeSIntF[F[_]: Sync](get: => CaAttribute[JInt]): F[Int] =
    safeAttributeF(get).map(_.toInt)

  def safeAttributeSListSIntF[F[_]: Sync](get: => CaAttribute[JInt]): F[List[Int]] =
    Nested(safeAttributeSListF(get)).map(_.toInt).value

  def safeAttributeList[F[_]: Sync, A](get: => CaAttribute[A]): F[Option[List[A]]] =
    Sync[F].delay(Option(get.values.asScala.toList))

  def safeAttributeSListSInt[F[_]: Sync, A](get: => CaAttribute[JInt]): F[Option[List[Int]]] =
    Nested(safeAttributeList(get)).map(_.map(_.toInt)).value

  def safeAttributeSListSDouble[F[_]: Sync, A](get: => CaAttribute[JDouble]): F[Option[List[Double]]] =
    Nested(safeAttributeList(get)).map(_.map(_.toDouble)).value

  def safeAttributeSListSFloat[F[_]: Sync, A](get: => CaAttribute[JFloat]): F[Option[List[Float]]] =
    Nested(safeAttributeList(get)).map(_.map(_.toFloat)).value

  /**
   * Decides to set a param comparing the current value and the value to be set
   * @param c Current value on the system
   * @param d Value to be set
   * @param f Action to set the parameter
   */
  def applyParam[F[_], A: Eq](c: A, d: A, f: A => F[Unit]): Option[F[Unit]] =
    if (c =!= d) f(d).some else none

  /**
   * Test if we should set a value d given that the current value c and
   * a given tolerance
   * @param t Max relative tolerance allowed between the current and destination value
   * @param c Current value on the system
   * @param d Value to be set
   */
  private def areValuesDifferentEnough(t: Double, c: Double, d: Double): Boolean =
    !(d === 0.0 && c === 0.0) && (d === 0.0 || abs((c - d)/d) > t)

  /**
   * Decides to set a param comparing the current value and the value to be set with
   * a given tolerance
   * @param relTolerance Max relative tolerance allowed between the current and destination value
   * @param c Current value on the system
   * @param d Value to be set
   * @param set Action to set the parameter
   */
  def applyParamT[F[_]: Functor](
    relTolerance: Double
  )(c: Double, d: Double, set: Double => F[Unit]): Option[F[Unit]] =
    if (areValuesDifferentEnough(relTolerance, c, d)) {
      set(d).some
    } else {
      none
    }

  // This method takes a list of actions returning possible actions
  // If at least one is defined it will execute them and then executes `after`
  def executeIfNeeded[F[_]: Monad, A](i:     List[F[Option[F[Unit]]]],
                                              after: F[A]): F[Unit] =
    i.sequence.flatMap { l =>
      val act: List[F[Unit]] = l.mapFilter(identity)
      (act.sequence *> after).whenA(act.nonEmpty)
    }

  class FOps[F[_]: Applicative, A](a: F[A]) {
    def wrapped: F[Option[F[A]]] =
      a.some.pure[F]
  }

  implicit def ToFOps[F[_]: Applicative, A](a: F[A]): FOps[F, A] =
    new FOps(a)

  // The return signature indicates this programs calculates if we maybe need an action
  // e.g. it checks that a value in epics compares to a reference and if so returns an optional
  // action
  def smartSetParamF[F[_]: Monad, A: Eq](v: A, get: F[A], set: F[Unit]): F[Option[F[Unit]]] =
    get.map(_ =!= v).map(_.option(set))

  def smartSetDoubleParamF[F[_]: Functor](relTolerance: Double)(v: Double, get: F[Double], set: F[Unit]): F[Option[F[Unit]]] =
    get.map(areValuesDifferentEnough(relTolerance, _, v).option(set))

  def defaultProgress[F[_]: Applicative](time: Time, remaining: RemainingTime, stage: ObserveStage): F[Progress] =
    ObsProgress(time, remaining, stage).pure[F].widen[Progress]

  def countdown[F[_]: Monad: Timer](
                                     total: Time,
                                     rem: F[Time],
                                     obsState: F[CarStateGeneric],
                                     obsStage: F[ObserveStage],
                                     p: (Time, RemainingTime, ObserveStage) => F[Progress]
                                   ): Stream[F, Progress] =
    ProgressUtil.fromFOption[F](_ =>
        for {
          c <- rem
          s <- obsState
          v <- obsStage
          r <- p(if(total>c) total else c, RemainingTime(c), v)
        } yield if(s.isBusy) r.some else none
    ).dropWhile(_.remaining.self.value === 0.0) // drop leading zeros
      .takeThrough(x => x.remaining.self.value > 0.0 || x.stage === ObserveStage.Acquiring ) // drop all tailing zeros but the first one, unless it is still acquiring

  // Component names read from instruments usually have a part name as suffix. For example, the
  // instrument may have had two K filters in its lifetime, one identified as K_G0804 and the
  // other as K_G0816. When it comes to find if the filter K is selected, we dont care about the
  // part name. This function removes it.
  def removePartName(s: String): String = {
    val pattern = "_G[0-9]{4}$"

    s.replaceAll(pattern, "")
  }

}
