// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.seqexec.odb.SeqFailure
import org.http4s.Uri

sealed trait SeqexecFailure

object SeqexecFailure {

  /** Seqexec does not know how to deal with instrument in sequence. */
  final case class UnrecognizedInstrument(name: String) extends SeqexecFailure

  /** Something went wrong while running a sequence. **/
  final case class Execution(errMsg: String) extends SeqexecFailure

  /** Exception thrown while running a sequence. */
  final case class SeqexecException(ex: Throwable) extends SeqexecFailure

  /** Exception thrown while running a sequence. */
  final case class SeqexecExceptionWhile(context: String, ex: Throwable) extends SeqexecFailure

  /** Invalid operation on a Sequence */
  final case class InvalidOp(errMsg: String) extends SeqexecFailure

  /** Indicates an unexpected problem while performing a Seqexec operation. */
  final case class Unexpected(msg: String) extends SeqexecFailure

  /** Indicates an attempt to enqueue an empty sequence */
  final case class EmptySequence(title: String) extends SeqexecFailure

  /** Timeout */
  final case class Timeout(msg: String) extends SeqexecFailure

  /** Sequence loading errors */
  final case class ODBSeqError(fail: SeqFailure) extends SeqexecFailure

  /** Exception thrown while communicating with the GDS */
  final case class GDSException(ex: Throwable, url: Uri) extends SeqexecFailure

  def explain(f: SeqexecFailure): String = f match {
    case UnrecognizedInstrument(name) => s"Unrecognized instrument: $name"
    case Execution(errMsg)            => s"Sequence execution failed with error: $errMsg"
    case SeqexecException(ex)         => s"Application exception: ${Option(ex.getMessage).getOrElse(ex.toString)}"
    case SeqexecExceptionWhile(c, e)  => s"Application exception while $c: ${Option(e.getMessage).getOrElse(e.toString)}"
    case InvalidOp(msg)               => s"Invalid operation: $msg"
    case Unexpected(msg)              => s"Unexpected error: $msg"
    case Timeout(msg)                 => s"Timeout while waiting for $msg"
    case EmptySequence(title)         => s"Attempt to enqueue empty sequence $title"
    case ODBSeqError(fail)            => SeqFailure.explain(fail)
    case GDSException(ex, url)        => s"Failure connecting with GDS at $url: ${ex.getMessage}"
  }

}
