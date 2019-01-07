// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.spModel.core.Peer
import edu.gemini.seqexec.odb.{SeqExecService, SeqexecSequence}
import edu.gemini.wdba.session.client.WDBA_XmlRpc_SessionClient
import gem.Observation
import seqexec.model.dhs.ImageFileId

class OdbProxy[F[_]](val loc: Peer, cmds: OdbProxy.OdbCommands[F]) {

  def host(): Peer = loc
  def read(oid: Observation.Id): Either[SeqexecFailure, SeqexecSequence] =
    SeqExecService.client(loc).sequence(new SPObservationID(oid.format)).leftMap(SeqexecFailure.OdbSeqError)

  val queuedSequences: SeqActionF[F, List[Observation.Id]] = cmds.queuedSequences()
  val datasetStart: (Observation.Id, String, ImageFileId) => SeqActionF[F, Boolean] = cmds.datasetStart
  val datasetComplete: (Observation.Id, String, ImageFileId) => SeqActionF[F, Boolean] = cmds.datasetComplete
  val obsAbort: (Observation.Id, String) => SeqActionF[F, Boolean] = cmds.obsAbort
  val sequenceEnd: Observation.Id => SeqActionF[F, Boolean] = cmds.sequenceEnd
  val sequenceStart: (Observation.Id, ImageFileId) => SeqActionF[F, Boolean] = cmds.sequenceStart
  val obsContinue: Observation.Id => SeqActionF[F, Boolean] = cmds.obsContinue
  val obsPause: (Observation.Id, String) => SeqActionF[F, Boolean] = cmds.obsPause
  val obsStop: (Observation.Id, String) => SeqActionF[F, Boolean] = cmds.obsStop

}

object OdbProxy {
  trait OdbCommands[F[_]] {
    def queuedSequences(): SeqActionF[F, List[Observation.Id]]
    def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqActionF[F, Boolean]
    def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqActionF[F, Boolean]
    def obsAbort(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean]
    def sequenceEnd(obsId: Observation.Id): SeqActionF[F, Boolean]
    def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): SeqActionF[F, Boolean]
    def obsContinue(obsId: Observation.Id): SeqActionF[F, Boolean]
    def obsPause(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean]
    def obsStop(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean]
  }

  final class DummyOdbCommands[F[_]: Sync] extends OdbCommands[F] {
    override def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqActionF[F, Boolean] = SeqActionF(false)
    override def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqActionF[F, Boolean] = SeqActionF(false)
    override def obsAbort(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean] = SeqActionF(false)
    override def sequenceEnd(obsId: Observation.Id): SeqActionF[F, Boolean] = SeqActionF(false)
    override def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): SeqActionF[F, Boolean] = SeqActionF(false)
    override def obsContinue(obsId: Observation.Id): SeqActionF[F, Boolean] = SeqActionF(false)
    override def obsPause(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean] = SeqActionF(false)
    override def obsStop(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean] = SeqActionF(false)
    override def queuedSequences(): SeqActionF[F, List[Observation.Id]] = SeqActionF(List.empty)
  }

  implicit class SeqexecSequenceOps(val s: SeqexecSequence) extends AnyVal {
    def stepsCount: Int = Option(s.config.getAllSteps).foldMap(_.length)
    def executedCount: Int = s.datasets.size
    def unExecutedSteps: Boolean = stepsCount =!= executedCount
  }

  final case class OdbCommandsImpl[F[_]: Sync](host: Peer) extends OdbCommands[F] {
    private val xmlrpcClient = new WDBA_XmlRpc_SessionClient(host.host, host.port.toString)
    private val sessionName = "sessionQueue"

    implicit class FRecover[A](t: F[A]) {
      def recover: F[Either[SeqexecFailure, A]] = t.attempt.map(_.leftMap(SeqexecFailure.SeqexecException))
    }

    override def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.datasetStart(sessionName, obsId.format, dataId, fileId.toString)
      ).recover
    )

    override def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.datasetComplete(sessionName, obsId.format, dataId, fileId.toString)
      ).recover
    )

    override def obsAbort(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.observationAbort(sessionName, obsId.format, reason)
      ).recover
    )

    override def sequenceEnd(obsId: Observation.Id): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.sequenceEnd(sessionName, obsId.format)
      ).recover
    )

    override def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.sequenceStart(sessionName, obsId.format, fileId.toString)
      ).recover
    )

    override def obsContinue(obsId: Observation.Id): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.observationContinue(sessionName, obsId.format)
      ).recover
    )

    override def obsPause(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.observationPause(sessionName, obsId.format, reason)
      ).recover
    )

    override def obsStop(obsId: Observation.Id, reason: String): SeqActionF[F, Boolean] = EitherT(
      Sync[F].delay(
        xmlrpcClient.observationStop(sessionName, obsId.format, reason)
      ).recover
    )

    override def queuedSequences(): SeqActionF[F, List[Observation.Id]] = EitherT(
      Sync[F].delay(
        xmlrpcClient.getObservations(sessionName).toList.flatMap(id => Observation.Id.fromString(id).toList)
      ).recover
    )
  }

}
