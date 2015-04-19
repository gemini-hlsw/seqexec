package edu.gemini.seqexec.shared

import edu.gemini.pot.sp.{ISPObservation, SPObservationID}
import edu.gemini.pot.spdb.IDBDatabaseService
import edu.gemini.seqexec.shared.SeqFailure.MissingObservation
import edu.gemini.spModel.config.ConfigBridge
import edu.gemini.spModel.config.map.ConfigValMapInstances.IDENTITY_MAP
import edu.gemini.spModel.config2.ConfigSequence

import scalaz._
import Scalaz._

/** Sequence Executor service server-side implementation. */
final class SeqExecServer(odb: IDBDatabaseService) extends SeqExecService {
  def lookup(oid: SPObservationID): TrySeq[ISPObservation] =
    Option(odb.lookupObservationByID(oid)) \/> MissingObservation(oid)

  override def sequence(oid: SPObservationID): TrySeq[ConfigSequence] =
    lookup(oid).map { obs =>
      ConfigBridge.extractSequence(obs, null, IDENTITY_MAP, true)
    }

}
