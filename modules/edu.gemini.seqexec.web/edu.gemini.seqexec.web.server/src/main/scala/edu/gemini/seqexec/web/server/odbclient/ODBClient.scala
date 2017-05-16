package edu.gemini.seqexec.web.server.odbclient

import scalaz.Kleisli
import knobs.Config
import scalaz.concurrent.Task
import org.http4s.client.blaze._
import org.http4s.Uri

case class ODBClientConfig(odbHost: String)

case class ODBClient(config: ODBClientConfig) {
  val httpClient = PooledHttp1Client()
  def observationTitle: Task[String] = {
    val baseUri = s"${config.odbHost}/odbbrowser/observations"
    val target = Uri.fromString(baseUri).toOption.get +?("programReference", "GS-2017A-Q-0")
    httpClient.expect[String](target)
  }
}

object ODBClient {
  def apply: Kleisli[Task, Config, ODBClient] = Kleisli { cfg: Config =>
    val odbHost = cfg.require[String]("seqexec-engine.odb")
    Task.delay(ODBClient(ODBClientConfig(odbHost)))
  }
}

