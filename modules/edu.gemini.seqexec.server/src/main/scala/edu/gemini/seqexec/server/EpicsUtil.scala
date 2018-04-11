// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.util
import java.util.concurrent.atomic.AtomicInteger
import java.util.{Timer, TimerTask}
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit.MILLISECONDS

import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.EpicsCommand.safe
import edu.gemini.seqexec.server.SeqexecFailure.SeqexecException
import org.log4s._
import squants.Time
import cats._
import cats.data.EitherT
import cats.effect.IO
import cats.implicits._

trait EpicsCommand {
  import EpicsCommand._

  protected val cs: Option[CaCommandSender]

  def post: SeqAction[Result] =
    safe {
      EitherT {
        IO.async[TrySeq[Result]] { (f: (Either[Throwable, TrySeq[Result]]) => Unit) =>
          cs.map { ccs =>
            ccs.postCallback {
              new CaCommandListener {
                override def onSuccess(): Unit = f(TrySeq(Completed).asRight)
                override def onPause(): Unit = f(TrySeq(Paused).asRight)
                override def onFailure(cause: Exception): Unit = f(cause.asLeft)
              }
            }
          // It should call f on all execution paths, thanks @tpolecat
          }.void.getOrElse(f(TrySeq.fail(SeqexecFailure.Unexpected("Unable to trigger command.")).asRight))
        }
      }
    }

  def mark: SeqAction[Unit] = safe(EitherT(IO.apply {
      cs.map(_.mark().asRight).getOrElse(SeqexecFailure.Unexpected("Unable to mark command.").asLeft)
    })
  )

  def setTimeout(t: Time): SeqAction[Unit] = EpicsUtil.setTimeout(cs.map(_.getApplySender), t)
}

trait EpicsSystem[T] {

  val className: String
  val Log: Logger
  val CA_CONFIG_FILE: String

  def build(service: CaService, tops: Map[String, String]): T

  // Still using a var, but at least now it's hidden. Attempts to access the single instance will
  // now result in an Exception with a meaningful message, instead of a NullPointerException
  private var instanceInternal = Option.empty[T] // scalastyle:ignore
  lazy val instance: T = instanceInternal.getOrElse(
    sys.error(s"Attempt to reference $className single instance before initialization."))

  def init(service: CaService, tops: Map[String, String]): TrySeq[Unit] = {
    try {
      tops.foldLeft((new XMLBuilder).fromStream(this.getClass.getResourceAsStream(CA_CONFIG_FILE))
        .withCaService(service))((b, a) => b.withTop(a._1, a._2)).buildAll()

        instanceInternal = Some(build(service, tops))

        TrySeq(())

    } catch {
      case c: Throwable =>
        Log.warn(c)(s"$className: Problem initializing EPICS service: ${c.getMessage}")
        TrySeq.fail(SeqexecFailure.SeqexecException(c))
    }
  }
}

@SuppressWarnings(Array("org.wartremover.warts.Overloading"))
object EpicsCommand {

  sealed trait Result
  object Paused extends Result
  object Completed extends Result

  def safe[A](a: SeqAction[A]): SeqAction[A] = EitherT(a.value.attempt.map {
    case Left(e)  => SeqexecException(e).asLeft
    case Right(r) => r
  })

  def setParameter[A,T](p: Option[CaParameter[T]], v: A, f: A => T): SeqAction[Unit] =
    safe(SeqAction.either {
      p.map(_.set(f(v)).asRight).getOrElse(SeqexecFailure.Unexpected("Unable to set parameter.").asLeft)
    } )

  def setParameter[T](p: Option[CaParameter[T]], v: T): SeqAction[Unit] =
    safe(SeqAction.either {
      p.map(_.set(v).asRight).getOrElse(SeqexecFailure.Unexpected("Unable to set parameter.").asLeft)
    } )

}

trait ObserveCommand {
  import ObserveCommand._

  protected val cs: Option[CaCommandSender]
  protected val os: Option[CaApplySender]

  def post: SeqAction[Result] =
    EpicsCommand.safe {
      EitherT {
        IO.async[TrySeq[Result]] { (f: (Either[Throwable, TrySeq[Result]]) => Unit) =>
          os.map { oos =>
            oos.postCallback {
              new CaCommandListener {
                override def onSuccess(): Unit = f(TrySeq(Success).asRight)
                override def onPause(): Unit = f(TrySeq(Paused).asRight)
                override def onFailure(cause: Exception): Unit = cause match {
                  case _: CaObserveStopped => f(TrySeq(Stopped).asRight)
                  case _: CaObserveAborted => f(TrySeq(Aborted).asRight)
                  case _                   => f(cause.asLeft)
                }
              }
            }
          }.void.getOrElse(f(TrySeq.fail(SeqexecFailure.Unexpected("Unable to trigger command.")).asRight))
        }
      }
    }

  def mark: SeqAction[Unit] = safe(EitherT(IO.apply {
    cs.map(_.mark().asRight).getOrElse(SeqexecFailure.Unexpected("Unable to mark command.").asLeft)
  }))

  def setTimeout(t: Time): SeqAction[Unit] = EpicsUtil.setTimeout(cs.map(_.getApplySender), t)
}

object ObserveCommand {
  sealed trait Result extends Product with Serializable
  case object Success extends Result
  case object Paused extends Result
  case object Stopped extends Result
  case object Aborted extends Result

  implicit val equal: Eq[Result] = Eq.fromUniversalEquals
}

object EpicsCodex {
  //This code deals with decoding and encoding the EPICS values
  trait EncodeEpicsValue[A, T] {
    def encode(a: A): T
  }

  object EncodeEpicsValue {
    def apply[A, T](f: A => T): EncodeEpicsValue[A, T] = (a: A) => f(a)
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

  def waitForValues[T](attr: CaAttribute[T], vv: Seq[T], timeout: Time, name: String): SeqAction[T] =
    EpicsCommand.safe(EitherT(IO.async[TrySeq[T]]((f) => {
      //The task is created with IO.async. So we do whatever we need to do,
      // and then call `f` to signal the completion of the task.

      //`resultGuard` and `lock` are used for synchronization.
      val resultGuard = new AtomicInteger(1)
      val lock = new ReentrantLock()

      // First we verify that the attribute doesn't already have the required value.
      if (!attr.values().isEmpty && vv.contains(attr.value)) {
        f(TrySeq(attr.value).asRight)
      } else {
        // If not, we set a timer for the timeout, and a listener for the EPICS
        // channel. The timer and the listener can both complete the IO. The
        // first one to do it cancels the other.The use of `resultGuard`
        // guarantees that only one of them will complete the IO.
        val timer = new Timer
        val statusListener = new CaAttributeListener[T] {
          override def onValueChange(newVals: util.List[T]): Unit = {
            if (!newVals.isEmpty && vv.contains(newVals.get(0)) && resultGuard.getAndDecrement() == 1) {
              locked(lock) {
                attr.removeListener(this)
                timer.cancel()
              }
              // This `right` looks a bit confusing because is not related to
              // the `TrySeq`, but to the result of `IO`.
              f(TrySeq(newVals.get(0)).asRight)
            }
          }

          override def onValidityChange(newValidity: Boolean): Unit = {}
        }

        locked(lock) {
          if (timeout.toMilliseconds.toLong > 0) {
            timer.schedule(new TimerTask {
              override def run(): Unit = if (resultGuard.getAndDecrement() == 1) {
                locked(lock) {
                  attr.removeListener(statusListener)
                }
                f(TrySeq.fail(SeqexecFailure.Timeout(s"waiting for $name.")).asRight)
              }
            }, timeout.toMilliseconds.toLong)
          }
          attr.addListener(statusListener)
        }
      }
    })))

  def waitForValue[T](attr: CaAttribute[T], v: T, timeout: Time, name: String): SeqAction[Unit] = waitForValues[T](attr, List(v), timeout, name).map(_ => ())

  def setTimeout(os: Option[CaApplySender], t: Time):SeqAction[Unit] = SeqAction.either{
    os.map(_.setTimeout(t.toMilliseconds.toLong, MILLISECONDS).asRight).getOrElse(SeqexecFailure.Unexpected("Unable to set timeout for EPICS command.").asLeft)
  }

  def smartSetParam[A: Eq](v: A, get: => Option[A], set: SeqAction[Unit]): SeqAction[Unit] =
    if(get =!= v.some) set else SeqAction.void

}