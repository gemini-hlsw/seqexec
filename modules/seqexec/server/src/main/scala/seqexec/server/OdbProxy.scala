// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Applicative
import cats.effect.Sync
import cats.data.EitherT
import cats.implicits._
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.spModel.core.Peer
import edu.gemini.seqexec.odb.{SeqExecService, SeqexecSequence}
import edu.gemini.wdba.session.client.WDBA_XmlRpc_SessionClient
import edu.gemini.wdba.xmlrpc.ServiceException
import io.chrisdavenport.log4cats.Logger
import gem.Observation
import seqexec.model.dhs._

sealed trait OdbCommands[F[_]] {
  def queuedSequences: F[List[Observation.Id]]
  def datasetStart(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean]
  def datasetComplete(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean]
  def obsAbort(obsId: Observation.Id, reason: String): F[Boolean]
  def sequenceEnd(obsId: Observation.Id): F[Boolean]
  def sequenceStart(obsId: Observation.Id, dataId: DataId): F[Boolean]
  def obsContinue(obsId: Observation.Id): F[Boolean]
  def obsPause(obsId: Observation.Id, reason: String): F[Boolean]
  def obsStop(obsId: Observation.Id, reason: String): F[Boolean]
}

sealed trait OdbProxy[F[_]] extends OdbCommands[F] {
  def read(oid: Observation.Id): F[SeqexecSequence]
  def queuedSequences: F[List[Observation.Id]]
}

object OdbProxy {
  def apply[F[_]: Sync](loc: Peer, cmds: OdbCommands[F]): OdbProxy[F] =
    new OdbProxy[F] {
      def read(oid: Observation.Id): F[SeqexecSequence] =
        EitherT(
          Sync[F].delay {
            SeqExecService
              .client(loc)
              .sequence(new SPObservationID(oid.format))
              .leftMap(SeqexecFailure.OdbSeqError)
          }).widenRethrowT

      def queuedSequences: F[List[Observation.Id]] = cmds.queuedSequences
      def datasetStart(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean] = cmds.datasetStart(obsId, dataId, fileId)
      def datasetComplete(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean] = cmds.datasetComplete(obsId, dataId, fileId)
      def obsAbort(obsId: Observation.Id, reason: String): F[Boolean] = cmds.obsAbort(obsId, reason)
      def sequenceEnd(obsId: Observation.Id): F[Boolean] = cmds.sequenceEnd(obsId)
      def sequenceStart(obsId: Observation.Id, dataId: DataId): F[Boolean] = cmds.sequenceStart(obsId, dataId)
      def obsContinue(obsId: Observation.Id): F[Boolean] = cmds.obsContinue(obsId)
      def obsPause(obsId: Observation.Id, reason: String): F[Boolean] = cmds.obsPause(obsId, reason)
      def obsStop(obsId: Observation.Id, reason: String): F[Boolean] = cmds.obsStop(obsId, reason)
    }

  final class DummyOdbCommands[F[_]: Applicative] extends OdbCommands[F] {
    override def datasetStart(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean] = true.pure[F]
    override def datasetComplete(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean] = true.pure[F]
    override def obsAbort(obsId: Observation.Id, reason: String): F[Boolean] = false.pure[F]
    override def sequenceEnd(obsId: Observation.Id): F[Boolean] = false.pure[F]
    override def sequenceStart(obsId: Observation.Id, dataId: DataId): F[Boolean] = false.pure[F]
    override def obsContinue(obsId: Observation.Id): F[Boolean] = false.pure[F]
    override def obsPause(obsId: Observation.Id, reason: String): F[Boolean] = false.pure[F]
    override def obsStop(obsId: Observation.Id, reason: String): F[Boolean] = false.pure[F]
    override def queuedSequences: F[List[Observation.Id]] = List.empty.pure[F]
  }

  implicit class SeqexecSequenceOps(val s: SeqexecSequence) extends AnyVal {
    def stepsCount: Int = Option(s.config.getAllSteps).foldMap(_.length)
    def executedCount: Int = s.datasets.size
    def unExecutedSteps: Boolean = stepsCount =!= executedCount
  }

  final case class OdbCommandsImpl[F[_]](host: Peer)(
    implicit val F: Sync[F], L: Logger[F]) extends OdbCommands[F] {
    private val xmlrpcClient = new WDBA_XmlRpc_SessionClient(host.host, host.port.toString)
    private val sessionName = "sessionQueue"

    override def datasetStart(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean] =
      L.debug(s"Start dataset for obsId: ${obsId.format} and dataId: $dataId") *>
      F.delay(
        xmlrpcClient.datasetStart(sessionName, obsId.format, dataId, fileId.toString)
      )

    override def datasetComplete(obsId: Observation.Id, dataId: DataId, fileId: ImageFileId): F[Boolean] =
      L.debug(s"Complete dataset for obsId: ${obsId.format} and dataId: $dataId") *>
      F.delay(
        xmlrpcClient.datasetComplete(sessionName, obsId.format, dataId, fileId.toString)
      )

    override def obsAbort(obsId: Observation.Id, reason: String): F[Boolean] =
      L.debug(s"Aborted: ${obsId.format} reason: $reason") *>
      F.delay(
        xmlrpcClient.observationAbort(sessionName, obsId.format, reason)
      )

    override def sequenceEnd(obsId: Observation.Id): F[Boolean] =
      L.debug(s"${obsId.format} sequence ended") *>
      F.delay(
        xmlrpcClient.sequenceEnd(sessionName, obsId.format)
      )

    override def sequenceStart(obsId: Observation.Id, dataId: DataId): F[Boolean] =
      L.debug(s"${obsId.format} sequence started") *>
      F.delay(
        xmlrpcClient.sequenceStart(sessionName, obsId.format, dataId.toString)
      )

    override def obsContinue(obsId: Observation.Id): F[Boolean] =
      L.debug(s"${obsId.format} sequence continues") *>
      F.delay(
        xmlrpcClient.observationContinue(sessionName, obsId.format)
      )

    override def obsPause(obsId: Observation.Id, reason: String): F[Boolean] =
      L.debug(s"${obsId.format} sequence paused: $reason") *>
      F.delay(
        xmlrpcClient.observationPause(sessionName, obsId.format, reason)
      )

    override def obsStop(obsId: Observation.Id, reason: String): F[Boolean] =
      L.debug(s"${obsId.format} sequence stopped: $reason") *>
      F.delay(
        xmlrpcClient.observationStop(sessionName, obsId.format, reason)
      )

    override def queuedSequences: F[List[Observation.Id]] =
      F.delay(
        xmlrpcClient.getObservations(sessionName).toList.flatMap(id => Observation.Id.fromString(id).toList)
      ).recoverWith {
        case e: ServiceException =>
          // We'll survive exceptions at the level of connecting to the wdba
          L.warn(e.getMessage) *> List.empty.pure[F]
      }
  }

}
