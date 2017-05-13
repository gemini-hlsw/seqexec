package edu.gemini.seqexec.server

import java.util.logging.Logger

import edu.gemini.epics.acm._
import edu.gemini.seqexec.server.SeqexecFailure.SeqexecException

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 10/5/15.
 */
trait EpicsCommand {
  import EpicsCommand._

  protected val cs: Option[CaCommandSender]

  def post: SeqAction[Unit] = safe(EitherT(Task.async[TrySeq[Unit]](f => cs.map(_.postCallback {
    new CaCommandListener {
      override def onSuccess(): Unit = f(TrySeq(()).right)

      override def onFailure(cause: Exception): Unit = f(cause.left)
    }
  } ).getOrElse(SeqexecFailure.Unexpected("Unable to trigger command.").left)
  ) ) )

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
  private var instanceInternal = Option.empty[T]
  lazy val instance: T = instanceInternal.getOrElse(
    throw new Exception(s"Attempt to reference $className single instance before initialization."))

  def init(service: CaService, tops: Map[String, String]): TrySeq[Unit] = {
    try {
      tops.foldLeft((new XMLBuilder).fromStream(this.getClass.getResourceAsStream(CA_CONFIG_FILE))
        .withCaService(service))((b, a) => b.withTop(a._1, a._2)).buildAll()

        instanceInternal = Some(build(service, tops))

        TrySeq(())

    } catch {
      case c: Throwable =>
        Log.warning(s"$className: Problem initializing EPICS service: " + c.getMessage + "\n"
          + c.getStackTrace.mkString("\n"))
        TrySeq.fail(SeqexecFailure.SeqexecException(c))
    }
  }
}

object EpicsCommand {
  def safe[A](a: SeqAction[A]): SeqAction[A] = EitherT(a.run.attempt.map {
    case -\/(t) => SeqexecException(t).left
    case \/-(e) => e
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
