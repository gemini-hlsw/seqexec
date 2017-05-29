package edu.gemini.seqexec.server.odbclient

import edu.gemini.seqexec.model.Model.SequenceId
import edu.gemini.seqexec.server.ConfigUtilOps.ExtractFailure
import edu.gemini.spModel.core.SPProgramID
import knobs.Config
import org.http4s.client.blaze._
import org.http4s.{Uri, scalaxml}

import scala.xml.Elem
import scalaz.{Kleisli, \/}
import scalaz.concurrent.Task

case class ODBClientConfig(odbHost: String, port: Int)

case class ODBClient(config: ODBClientConfig) {
  val httpClient = PooledHttp1Client()
  // Entity Decoder for xml
  implicit val decoder = scalaxml.xml()

  def observationTitle(id: SPProgramID, obsId: SequenceId): Task[ExtractFailure \/ String] = {
    val baseUri = s"http://${config.odbHost}:${config.port}/odbbrowser/observations"
    val target = Uri.fromString(baseUri).toOption.get +?("programReference", id.stringValue)
    httpClient.expect[Elem](target).map { xml =>
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

