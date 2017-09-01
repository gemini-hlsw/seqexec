// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.SeqexecFailure.SeqexecException

import org.log4s._
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 10/5/15.
 */
trait EpicsCommand {
  import EpicsCommand._

  protected val cs: Option[CaCommandSender]

  def post: SeqAction[Unit] =
    safe {
      EitherT {
        Task.async[TrySeq[Unit]] { (f: (Throwable \/ TrySeq[Unit]) => Unit) =>
          cs.map { ccs =>
            ccs.postCallback {
              new CaCommandListener {
                override def onSuccess(): Unit = f(TrySeq(()).right)
                override def onFailure(cause: Exception): Unit = f(cause.left)
              }
            }
          // It should call f on all execution paths, thanks @tpolecat
          }.void.getOrElse(f(SeqexecFailure.Unexpected("Unable to trigger command.").left.right))
        }
      }
    }

  def mark: SeqAction[Unit] = safe(EitherT(Task.delay {
      cs.map(_.mark().right).getOrElse(SeqexecFailure.Unexpected("Unable to mark command.").left)
    })
  )
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
  def safe[A](a: SeqAction[A]): SeqAction[A] = EitherT(a.run.handle {
    case e: Exception => SeqexecException(e).left
  })

  def setParameter[A,T](p: Option[CaParameter[T]], v: A, f: A => T): SeqAction[Unit] =
    safe(SeqAction.either {
      p.map(_.set(f(v)).right).getOrElse(SeqexecFailure.Unexpected("Unable to set parameter.").left)
    } )

  def setParameter[T](p: Option[CaParameter[T]], v: T): SeqAction[Unit] =
    safe(SeqAction.either {
      p.map(_.set(v).right).getOrElse(SeqexecFailure.Unexpected("Unable to set parameter.").left)
    } )

}

object EpicsCodex {
  //This code deals with decoding and encoding the EPICS values
  trait EncodeEpicsValue[A, T] {
    def encode(a: A): T
  }

  object EncodeEpicsValue {
    def apply[A, T](f: A => T): EncodeEpicsValue[A, T] = new EncodeEpicsValue[A, T] {
      override def encode(a: A): T = f(a)
    }
  }

  def encode[A, T](a: A)(implicit e: EncodeEpicsValue[A, T]): T = e.encode(a)

  trait DecodeEpicsValue[T, A] {
    def decode(t: T): A
  }

  object DecodeEpicsValue {
    def apply[T, A](f: T => A): DecodeEpicsValue[T, A] = new DecodeEpicsValue[T, A] {
      override def decode(t: T): A = f(t)
    }
  }

  def decode[T, A](t: T)(implicit e: DecodeEpicsValue[T, A]): A = e.decode(t)

}
