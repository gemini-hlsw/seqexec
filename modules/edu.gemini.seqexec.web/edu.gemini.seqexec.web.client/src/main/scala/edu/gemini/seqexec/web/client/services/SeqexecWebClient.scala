package edu.gemini.seqexec.web.client.services

import edu.gemini.seqexec.web.common.{Sequence, SequenceSteps}
import org.scalajs.dom.ext.Ajax
import upickle.default

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Encapsulate remote calls to the Seqexec Web API
  */
object SeqexecWebClient {
  val baseUrl = "/api"

  def read(id: String): Future[Sequence] = {
    Ajax.get(
      url = s"$baseUrl/sequence/$id"
    ).map(s => default.read[Sequence](s.responseText))
  }
}
