package edu.gemini.seqexec.server

import edu.gemini.epics.acm.{CaCommandListener, CaCommandSender, CaParameter}
import edu.gemini.seqexec.server.SeqexecFailure.{Timeout, SeqexecException}

import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
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
      cs.map(_.mark.right).getOrElse(SeqexecFailure.Unexpected("Unable to mark command.").left)
    })
  )
}

object EpicsCommand {
  def safe[A](a: SeqAction[A]): SeqAction[A] = EitherT(a.run.attempt.map {
    case -\/(t) => SeqexecException(t).left
    case \/-(e) => e
  })

  def setParameter[T](p: Option[CaParameter[T]], v: T): SeqAction[Unit] =
    safe(EitherT(Task.delay {
      p.map(_.set(v).right).getOrElse(SeqexecFailure.Unexpected("Unable to set parameter.").left)
    } ) )

}
