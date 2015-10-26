package edu.gemini.seqexec.server

/**
 * Created by jluhrs on 5/18/15.
 */
sealed trait SeqexecFailure

object SeqexecFailure {

  /** Seqexec does not know how to deal with instrument in sequence. */
  case class UnrecognizedInstrument(name: String) extends SeqexecFailure

  /** Something went wrong while running a sequence. **/
  case class Execution(errMsg: String) extends SeqexecFailure

  /** Exception thrown while running a sequence. */
  case class SeqexecException(ex: Throwable) extends SeqexecFailure

  /** Invalid operation on a Sequence */
  case class InvalidOp(errMsg: String) extends SeqexecFailure

  /** Indicates an unexpected problem while performing a Seqexec operation. */
  case class Unexpected(msg: String) extends SeqexecFailure

  /** Timeout */
  case class Timeout(msg: String) extends SeqexecFailure

  def explain(f: SeqexecFailure): String = f match {
    case UnrecognizedInstrument(name) => s"Unrecognized instrument: $name"
    case Execution(errMsg)            => s"Sequence execution failed with error $errMsg"
    case SeqexecException(ex)         => "Application exception: " + ex.getMessage
    case InvalidOp(msg)               => s"Invalid operation: $msg"
    case Unexpected(msg)              => s"Unexpected error: $msg"
    case Timeout(msg)                 => s"Timeout while waiting for $msg"
  }

}
