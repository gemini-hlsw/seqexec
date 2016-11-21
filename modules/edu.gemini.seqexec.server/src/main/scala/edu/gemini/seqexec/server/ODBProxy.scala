package edu.gemini.seqexec.server

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.odb.SeqExecService
import edu.gemini.spModel.config2.ConfigSequence
import edu.gemini.spModel.core.Peer

import scalaz.\/

/**
  * Created by jluhrs on 9/6/16.
  */
class ODBProxy(val loc: Peer) {

  def host(): Peer = loc
  def read(oid: SPObservationID): SeqexecFailure \/ ConfigSequence =
    SeqExecService.client(loc).sequence(oid).leftMap(SeqexecFailure.ODBSeqError)

}
