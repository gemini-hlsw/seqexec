// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.spModel.core.Peer
import edu.gemini.seqexec.odb.{SeqExecService, SeqexecSequence}
import edu.gemini.wdba.session.client.WDBA_XmlRpc_SessionClient
import gem.Observation
import seqexec.model.dhs.ImageFileId

class ODBProxy(val loc: Peer, cmds: ODBProxy.OdbCommands) {

  def host(): Peer = loc
  def read(oid: Observation.Id): Either[SeqexecFailure, SeqexecSequence] =
    SeqExecService.client(loc).sequence(new SPObservationID(oid.format)).leftMap(SeqexecFailure.ODBSeqError)

  val queuedSequences: SeqAction[List[Observation.Id]] = cmds.queuedSequences()
  val datasetStart: (Observation.Id, String, ImageFileId) => SeqAction[Boolean] = cmds.datasetStart
  val datasetComplete: (Observation.Id, String, ImageFileId) => SeqAction[Boolean] = cmds.datasetComplete
  val obsAbort: (Observation.Id, String) => SeqAction[Boolean] = cmds.obsAbort
  val sequenceEnd: Observation.Id => SeqAction[Boolean] = cmds.sequenceEnd
  val sequenceStart: (Observation.Id, ImageFileId) => SeqAction[Boolean] = cmds.sequenceStart
  val obsContinue: Observation.Id => SeqAction[Boolean] = cmds.obsContinue
  val obsPause: (Observation.Id, String) => SeqAction[Boolean] = cmds.obsPause
  val obsStop: (Observation.Id, String) => SeqAction[Boolean] = cmds.obsStop

}

object ODBProxy {
  trait OdbCommands {
    def queuedSequences(): SeqAction[List[Observation.Id]]
    def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqAction[Boolean]
    def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqAction[Boolean]
    def obsAbort(obsId: Observation.Id, reason: String): SeqAction[Boolean]
    def sequenceEnd(obsId: Observation.Id): SeqAction[Boolean]
    def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): SeqAction[Boolean]
    def obsContinue(obsId: Observation.Id): SeqAction[Boolean]
    def obsPause(obsId: Observation.Id, reason: String): SeqAction[Boolean]
    def obsStop(obsId: Observation.Id, reason: String): SeqAction[Boolean]
  }

  object DummyOdbCommands extends OdbCommands {
    override def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = SeqAction(false)
    override def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = SeqAction(false)
    override def obsAbort(obsId: Observation.Id, reason: String): SeqAction[Boolean] = SeqAction(false)
    override def sequenceEnd(obsId: Observation.Id): SeqAction[Boolean] = SeqAction(false)
    override def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): SeqAction[Boolean] = SeqAction(false)
    override def obsContinue(obsId: Observation.Id): SeqAction[Boolean] = SeqAction(false)
    override def obsPause(obsId: Observation.Id, reason: String): SeqAction[Boolean] = SeqAction(false)
    override def obsStop(obsId: Observation.Id, reason: String): SeqAction[Boolean] = SeqAction(false)
    override def queuedSequences(): SeqAction[List[Observation.Id]] = SeqAction(List.empty)
  }

  implicit class SeqexecSequenceOps(val s: SeqexecSequence) extends AnyVal {
    def stepsCount: Int = Option(s.config.getAllSteps).map(_.length).getOrElse(0)
    def executedCount: Int = s.datasets.size
    def unExecutedSteps: Boolean = stepsCount =!= executedCount
  }

  final case class OdbCommandsImpl(host: Peer) extends OdbCommands {
    private val xmlrpcClient = new WDBA_XmlRpc_SessionClient(host.host, host.port.toString)
    private val sessionName = "sessionQueue"

    implicit class IORecover[A](t: IO[A]) {
      def recover: IO[Either[SeqexecFailure, A]] = t.map(_.asRight).attempt.map {
        case Left(e)  => SeqexecFailure.SeqexecException(e).asLeft
        case Right(r) => r
      }
    }

    override def datasetStart(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.datasetStart(sessionName, obsId.format, dataId, fileId.toString)
      ).recover
    )

    override def datasetComplete(obsId: Observation.Id, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.datasetComplete(sessionName, obsId.format, dataId, fileId.toString)
      ).recover
    )

    override def obsAbort(obsId: Observation.Id, reason: String): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationAbort(sessionName, obsId.format, reason)
      ).recover
    )

    override def sequenceEnd(obsId: Observation.Id): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.sequenceEnd(sessionName, obsId.format)
      ).recover
    )

    override def sequenceStart(obsId: Observation.Id, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.sequenceStart(sessionName, obsId.format, fileId.toString)
      ).recover
    )

    override def obsContinue(obsId: Observation.Id): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationContinue(sessionName, obsId.format)
      ).recover
    )

    override def obsPause(obsId: Observation.Id, reason: String): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationPause(sessionName, obsId.format, reason)
      ).recover
    )

    override def obsStop(obsId: Observation.Id, reason: String): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationStop(sessionName, obsId.format, reason)
      ).recover
    )

    override def queuedSequences(): SeqAction[List[Observation.Id]] = EitherT(
      IO.apply(
        xmlrpcClient.getObservations(sessionName).toList.flatMap(id => Observation.Id.fromString(id).toList)
      ).recover
    )
  }

}
