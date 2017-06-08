package edu.gemini.seqexec.server

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.spModel.config2.ConfigSequence
import edu.gemini.spModel.core.Peer
import edu.gemini.spModel.core.SPProgramID
import edu.gemini.seqexec.odb.SeqExecService
import edu.gemini.seqexec.model.Model.SequenceId
import edu.gemini.seqexec.server.ConfigUtilOps.ExtractFailure
import edu.gemini.wdba.session.client.WDBA_XmlRpc_SessionClient
import edu.gemini.seqexec.engine.Action
import edu.gemini.seqexec.model.dhs.ImageFileId

import scalaz.{Kleisli, EitherT, \/}
import scalaz.concurrent.Task
import scala.xml.{Elem, XML}
import org.http4s.client.blaze._
import org.http4s.{Uri, scalaxml}
import knobs.Config

/**
  * Created by jluhrs on 9/6/16.
  */
class ODBProxy(val loc: Peer, cmds: ODBProxy.OdbCommands) {

  def host(): Peer = loc
  def read(oid: SPObservationID): SeqexecFailure \/ ConfigSequence =
    SeqExecService.client(loc).sequence(oid).leftMap(SeqexecFailure.ODBSeqError)

  val queuedSequences: () => SeqAction[Seq[SPObservationID]] = cmds.queuedSequences
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

  case class OdbCommandsImpl(host: Peer) extends OdbCommands {
    val xmlrpcClient = new WDBA_XmlRpc_SessionClient(host.host, host.port.toString)
    val sessionName = "sessionQueue"

    override def datasetStart(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.datasetStart(sessionName, obsId.stringValue, dataId, fileId.toString)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def datasetComplete(obsId: SPObservationID, dataId: String, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.datasetComplete(sessionName, obsId.stringValue, dataId, fileId.toString)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def obsAbort(obsId: SPObservationID, reason: String): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.observationAbort(sessionName, obsId.stringValue, reason)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def sequenceEnd(obsId: SPObservationID): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.sequenceEnd(sessionName, obsId.stringValue)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def sequenceStart(obsId: SPObservationID, fileId: ImageFileId): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.sequenceStart(sessionName, obsId.stringValue, fileId.toString)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def obsContinue(obsId: SPObservationID): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.observationContinue(sessionName, obsId.stringValue)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def obsPause(obsId: SPObservationID, reason: String): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.observationPause(sessionName, obsId.stringValue, reason)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def obsStop(obsId: SPObservationID, reason: String): SeqAction[Boolean] = EitherT(
      Task.delay(
        xmlrpcClient.observationStop(sessionName, obsId.stringValue, reason)
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )

    override def queuedSequences(): SeqAction[Seq[SPObservationID]] = EitherT(
      Task.delay(
        xmlrpcClient.getObservations(sessionName).toList.map(new SPObservationID(_))
      ).attempt.map(_.leftMap(e => SeqexecFailure.SeqexecException(e)))
    )
  }

}

case class ODBClientConfig(odbHost: String, port: Int)

case class ODBClient(config: ODBClientConfig) {
  val httpClient = PooledHttp1Client()
  // Entity Decoder for xml
  implicit val decoder = scalaxml.xml()

  def observationTitle(id: SPProgramID, obsId: SequenceId): Task[ExtractFailure \/ String] = {
    val baseUri = s"http://${config.odbHost}:${config.port}/odbbrowser/observations"
    val target = Uri.fromString(baseUri).toOption.get +?("programReference", id.stringValue)
    httpClient.expect[String](target).map { xmlString =>
      // Parse the xml explicitly
      val xml = XML.loadString(xmlString)
      for {
        x <- xml \\ "observations" \ "observation"
        if (x \ "id").text == obsId
        n <- x \ "name"
      } yield n
    }.map(_.text).map(\/.right)
  }
}

object ODBClient {
  val DefaultODBBrowserPort: Int = 8442
  def apply: Kleisli[Task, Config, ODBClient] = Kleisli { cfg: Config =>
    val odbHost = cfg.require[String]("seqexec-engine.odb")
    Task.delay(ODBClient(ODBClientConfig(odbHost, DefaultODBBrowserPort)))
  }
}

