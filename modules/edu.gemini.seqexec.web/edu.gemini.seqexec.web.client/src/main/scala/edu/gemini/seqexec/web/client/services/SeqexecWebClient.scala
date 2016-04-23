package edu.gemini.seqexec.web.client.services

import edu.gemini.seqexec.web.common.{HttpStatusCodes, RegularCommand, SeqexecQueue, Sequence}
import org.scalajs.dom.ext.{Ajax, AjaxException}
import upickle.default

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Encapsulates remote calls to the Seqexec Web API
  */
object SeqexecWebClient {
  val baseUrl = "/api/seqexec"

  def read(id: String): Future[List[Sequence]] =
    Ajax.get(
      url = s"$baseUrl/sequence/$id"
    )
    .map(s => default.read[List[Sequence]](s.responseText))
    .recover {
      case AjaxException(xhr) if xhr.status == HttpStatusCodes.NotFound  => Nil // If not found, we'll consider it like an empty response
    }

  def readQueue(): Future[SeqexecQueue] =
    Ajax.get(
      url = s"$baseUrl/current/queue"
    ).map(s => default.read[SeqexecQueue](s.responseText))

  def run(s: Sequence): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/${s.id}/run"
    ).map(s => default.read[RegularCommand](s.responseText))
  }
}
