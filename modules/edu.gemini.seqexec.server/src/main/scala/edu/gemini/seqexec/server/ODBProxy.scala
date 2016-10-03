package edu.gemini.seqexec.server

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.odb.SeqExecService
import edu.gemini.spModel.config2.ConfigSequence
import edu.gemini.spModel.core.Peer

import scalaz.\/

/**
  * Created by jluhrs on 9/6/16.
  */
object ODBProxy {
  private var loc = new Peer("localhost", 8443, null)

  def host(): Peer = loc
  def host(l: Peer): Unit = { loc = l }
  def read(oid: SPObservationID): SeqexecFailure \/ ConfigSequence =
    SeqExecService.client(ODBProxy.host()).sequence(oid).leftMap(SeqexecFailure.ODBSeqError)

}
