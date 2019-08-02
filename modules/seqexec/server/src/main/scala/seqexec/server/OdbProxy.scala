// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Applicative
import cats.effect.Sync
import cats.implicits._
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.spModel.core.Peer
import edu.gemini.seqexec.odb.{SeqExecService, SeqexecSequence}
import edu.gemini.wdba.session.client.WDBA_XmlRpc_SessionClient
import edu.gemini.wdba.xmlrpc.ServiceException
import gem.Observation
import seqexec.model.dhs.ImageFileId
import org.log4s.getLogger

class OdbProxy[F[_]: Sync](val loc: Peer, cmds: OdbProxy.OdbCommands[F]) {

  def host(): Peer = loc
  def read(oid: Observation.Id): F[Either[SeqexecFailure, SeqexecSequence]] =
    Sync[F].delay {
      SeqExecService.client(loc).sequence(new SPObservationID(oid.format)).leftMap(SeqexecFailure.OdbSeqError)
    }

  val queuedSequences: F[List[Observation.Id]] = cmds.queuedSequences()
  val datasetStart: (Observation.Id, String, ImageFileId) => F[Boolean] = cmds.datasetStart
  val datasetComplete: (Observation.Id, String, ImageFileId) => F[Boolean] = cmds.datasetComplete
  val obsAbort: (Observation.Id, String) => F[Boolean] = cmds.obsAbort
  val sequenceEnd: Observation.Id => F[Boolean] = cmds.sequenceEnd
  val sequenceStart: (Observation.Id, ImageFileId) => F[Boolean] = cmds.sequenceStart
  val obsContinue: Observation.Id => F[Boolean] = cmds.obsContinue
  val obsPause: (Observation.Id, String) => F[Boolean] = cmds.obsPause
  val obsStop: (Observation.Id, String) => F[Boolean] = cmds.obsStop

}

object OdbProxy {
  private val Log = getLogger

  trait OdbCommands[F[_]] {
    def queuedSequences(): F[List[Observation.Id]]
    def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): F[Boolean]
    def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): F[Boolean]
    def obsAbort(obsId: Observation.Id, reason: String): F[Boolean]
    def sequenceEnd(obsId: Observation.Id): F[Boolean]
    def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): F[Boolean]
    def obsContinue(obsId: Observation.Id): F[Boolean]
    def obsPause(obsId: Observation.Id, reason: String): F[Boolean]
    def obsStop(obsId: Observation.Id, reason: String): F[Boolean]
  }

  final class DummyOdbCommands[F[_]: Applicative] extends OdbCommands[F] {
    override def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): F[Boolean] = false.pure[F]
    override def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): F[Boolean] = false.pure[F]
    override def obsAbort(obsId: Observation.Id, reason: String): F[Boolean] = false.pure[F]
    override def sequenceEnd(obsId: Observation.Id): F[Boolean] = false.pure[F]
    override def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] = false.pure[F]
    override def obsContinue(obsId: Observation.Id): F[Boolean] = false.pure[F]
    override def obsPause(obsId: Observation.Id, reason: String): F[Boolean] = false.pure[F]
    override def obsStop(obsId: Observation.Id, reason: String): F[Boolean] = false.pure[F]
    override def queuedSequences(): F[List[Observation.Id]] = List.empty.pure[F]
  }

  implicit class SeqexecSequenceOps(val s: SeqexecSequence) extends AnyVal {
    def stepsCount: Int = Option(s.config.getAllSteps).foldMap(_.length)
    def executedCount: Int = s.datasets.size
    def unExecutedSteps: Boolean = stepsCount =!= executedCount
  }

  final case class OdbCommandsImpl[F[_]](host: Peer)(implicit val F: Sync[F]) extends OdbCommands[F] {
    private val xmlrpcClient = new WDBA_XmlRpc_SessionClient(host.host, host.port.toString)
    private val sessionName = "sessionQueue"

    implicit class FRecover[A](t: F[A]) {
      def recover: F[Either[SeqexecFailure, A]] = t.attempt.map(_.leftMap(SeqexecFailure.SeqexecException))
    }

    override def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): F[Boolean] =
      F.delay(
        xmlrpcClient.datasetStart(sessionName, obsId.format, dataId, fileId.toString)
      )

    override def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): F[Boolean] =
      F.delay(
        xmlrpcClient.datasetComplete(sessionName, obsId.format, dataId, fileId.toString)
      )

    override def obsAbort(obsId: Observation.Id, reason: String): F[Boolean] =
      F.delay(
        xmlrpcClient.observationAbort(sessionName, obsId.format, reason)
      )

    override def sequenceEnd(obsId: Observation.Id): F[Boolean] =
      Sync[F].delay(
        xmlrpcClient.sequenceEnd(sessionName, obsId.format)
      )

    override def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): F[Boolean] =
      F.delay(
        xmlrpcClient.sequenceStart(sessionName, obsId.format, fileId.toString)
      )

    override def obsContinue(obsId: Observation.Id): F[Boolean] =
      Sync[F].delay(
        xmlrpcClient.observationContinue(sessionName, obsId.format)
      )

    override def obsPause(obsId: Observation.Id, reason: String): F[Boolean] =
      Sync[F].delay(
        xmlrpcClient.observationPause(sessionName, obsId.format, reason)
      )

    override def obsStop(obsId: Observation.Id, reason: String): F[Boolean] =
      Sync[F].delay(
        xmlrpcClient.observationStop(sessionName, obsId.format, reason)
      )

    override def queuedSequences(): F[List[Observation.Id]] =
      Sync[F].delay(
        xmlrpcClient.getObservations(sessionName).toList.flatMap(id => Observation.Id.fromString(id).toList)
      ).recoverWith {
        case e: ServiceException =>
          // We'll survive exceptions at the level of connecting to the wdba
          Sync[F].delay(Log.warn(e.getMessage)) *> List.empty.pure[F]
      }
  }

}
