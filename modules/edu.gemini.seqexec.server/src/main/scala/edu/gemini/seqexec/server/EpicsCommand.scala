package edu.gemini.seqexec.server

import edu.gemini.epics.acm.{CaCommandListener, CaCommandSender, CaParameter}
import edu.gemini.seqexec.server.SeqexecFailure.SeqexecException

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 10/5/15.
 */
trait EpicsCommand {

  protected val cs: Option[CaCommandSender]

  final protected def safe[A](a: SeqAction[A]): SeqAction[A] = a.attempt.map {
    case -\/(t) => SeqexecException(t).left
    case \/-(e) => e
  }

  final protected def setParameter[T](p: Option[CaParameter[T]], v: T): SeqAction[Unit] =
    safe(Task.delay {
      p.map(_.set(v).right).getOrElse(SeqexecFailure.Unexpected("Unable to set parameter.").left)
    }
    )

  def post(): SeqAction[Unit] = safe(Task.async(f => cs.map(_.postCallback {
    new CaCommandListener {
      override def onSuccess(): Unit = f(TrySeq(()).right)

      override def onFailure(cause: Exception): Unit = f(cause.left)
    }
  } ).getOrElse(SeqexecFailure.Unexpected("Unable to trigger command.").left)
  ) )
}
