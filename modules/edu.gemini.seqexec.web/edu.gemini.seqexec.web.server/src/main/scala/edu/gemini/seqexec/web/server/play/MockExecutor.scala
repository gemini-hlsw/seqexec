package edu.gemini.seqexec.web.server.play

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.SeqexecFailure
import edu.gemini.spModel.config2.ConfigSequence

import scalaz._
import Scalaz._

/** Mock of ExecutorImpl */
object MockExecutor {
  def read(oid: SPObservationID):SeqexecFailure \/ ConfigSequence = \/-(new ConfigSequence)
}
