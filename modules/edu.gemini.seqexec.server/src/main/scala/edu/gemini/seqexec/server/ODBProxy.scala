// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import cats.data.EitherT
import cats.effect.IO
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.spModel.core.Peer
import edu.gemini.seqexec.odb.{SeqExecService, SeqexecSequence}
import edu.gemini.wdba.session.client.WDBA_XmlRpc_SessionClient
import edu.gemini.seqexec.model.dhs.ImageFileId
import cats.implicits._

class ODBProxy(val loc: Peer, cmds: ODBProxy.OdbCommands) {

  def host(): Peer = loc
  def read(oid: SPObservationID): Either[SeqexecFailure, SeqexecSequence] =
    SeqExecService.client(loc).sequence(oid).leftMap(SeqexecFailure.ODBSeqError)

  val queuedSequences: SeqAction[Seq[SPObservationID]] = cmds.queuedSequences()
  val datasetStart: (SPObservationID, String, ImageFileId) => SeqAction[Boolean] = cmds.datasetStart
  val datasetComplete: (SPObservationID, String, ImageFileId) => SeqAction[Boolean] = cmds.datasetComplete
  val obsAbort: (SPObservationID, String) => SeqAction[Boolean] = cmds.obsAbort
  val sequenceEnd: (SPObservationID) => SeqAction[Boolean] = cmds.sequenceEnd
  val sequenceStart: (SPObservationID, ImageFileId) => SeqAction[Boolean] = cmds.sequenceStart
  val obsContinue: (SPObservationID) => SeqAction[Boolean] = cmds.obsContinue
  val obsPause: (SPObservationID, String) => SeqAction[Boolean] = cmds.obsPause
  val obsStop: (SPObservationID, String) => SeqAction[Boolean] = cmds.obsStop

}

object ODBProxy {
  trait OdbCommands {
    def queuedSequences(): SeqAction[Seq[SPObservationID]]
    def datasetStart(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean]
    def datasetComplete(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean]
    def obsAbort(obsId: SPObservationID, reason: String): SeqAction[Boolean]
    def sequenceEnd(obsId: SPObservationID): SeqAction[Boolean]
    def sequenceStart(obsId: SPObservationID, fileId: ImageFileId): SeqAction[Boolean]
    def obsContinue(obsId: SPObservationID): SeqAction[Boolean]
    def obsPause(obsId: SPObservationID, reason: String): SeqAction[Boolean]
    def obsStop(obsId: SPObservationID, reason: String): SeqAction[Boolean]
  }

  object DummyOdbCommands extends OdbCommands {
    override def datasetStart(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = SeqAction(false)
    override def datasetComplete(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = SeqAction(false)
    override def obsAbort(obsId: SPObservationID, reason: String): SeqAction[Boolean] = SeqAction(false)
    override def sequenceEnd(obsId: SPObservationID): SeqAction[Boolean] = SeqAction(false)
    override def sequenceStart(obsId: SPObservationID, fileId: ImageFileId): SeqAction[Boolean] = SeqAction(false)
    override def obsContinue(obsId: SPObservationID): SeqAction[Boolean] = SeqAction(false)
    override def obsPause(obsId: SPObservationID, reason: String): SeqAction[Boolean] = SeqAction(false)
    override def obsStop(obsId: SPObservationID, reason: String): SeqAction[Boolean] = SeqAction(false)
    override def queuedSequences(): SeqAction[Seq[SPObservationID]] = SeqAction(List.empty)
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
        case Left(e: Exception) => SeqexecFailure.SeqexecException(e).asLeft
        case Right(r)           => r
      }
    }

    override def datasetStart(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.datasetStart(sessionName, obsId.stringValue, dataId, fileId.toString)
      ).recover
    )

    override def datasetComplete(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.datasetComplete(sessionName, obsId.stringValue, dataId, fileId.toString)
      ).recover
    )

    override def obsAbort(obsId: SPObservationID, reason: String): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationAbort(sessionName, obsId.stringValue, reason)
      ).recover
    )

    override def sequenceEnd(obsId: SPObservationID): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.sequenceEnd(sessionName, obsId.stringValue)
      ).recover
    )

    override def sequenceStart(obsId: SPObservationID, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.sequenceStart(sessionName, obsId.stringValue, fileId.toString)
      ).recover
    )

    override def obsContinue(obsId: SPObservationID): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationContinue(sessionName, obsId.stringValue)
      ).recover
    )

    override def obsPause(obsId: SPObservationID, reason: String): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationPause(sessionName, obsId.stringValue, reason)
      ).recover
    )

    override def obsStop(obsId: SPObservationID, reason: String): SeqAction[Boolean] = EitherT(
      IO.apply(
        xmlrpcClient.observationStop(sessionName, obsId.stringValue, reason)
      ).recover
    )

    override def queuedSequences(): SeqAction[Seq[SPObservationID]] = EitherT(
      IO.apply(
        xmlrpcClient.getObservations(sessionName).toSeq.map(new SPObservationID(_))
      ).recover
    )
  }

}
