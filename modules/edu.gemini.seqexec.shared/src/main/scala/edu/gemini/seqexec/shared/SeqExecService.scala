package edu.gemini.seqexec.shared

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.shared.SeqFailure.SeqException


import edu.gemini.spModel.config2.ConfigSequence
import edu.gemini.spModel.core.Peer
import edu.gemini.util.trpc.client.TrpcClient

import scalaz._
import Scalaz._

/**
 * Sequence Executor Service API.
 */
trait SeqExecService {

  /** Fetches the sequence associated with the given observation id, if it
    * exists. */
  def sequence(oid: SPObservationID): TrySeq[ConfigSequence]
}

object SeqExecService {
  def client(peer: Peer): SeqExecService = new SeqExecService {
    val trpc = TrpcClient(peer).withoutKeys

    private def call[A](op: SeqExecService => TrySeq[A]): TrySeq[A] =
      trpc { remote => op(remote[SeqExecService]) }.valueOr(ex => SeqException(ex).left)

    override def sequence(oid: SPObservationID): TrySeq[ConfigSequence] =
      call(_.sequence(oid))
  }
}