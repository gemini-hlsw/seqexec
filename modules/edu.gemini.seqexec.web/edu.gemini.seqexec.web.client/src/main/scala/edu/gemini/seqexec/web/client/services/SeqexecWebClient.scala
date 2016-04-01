package edu.gemini.seqexec.web.client.services

import edu.gemini.seqexec.web.common.{SeqexecQueue, Sequence, SequenceSteps}
import org.scalajs.dom.ext.Ajax
import upickle.default

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Encapsulates remote calls to the Seqexec Web API
  */
object SeqexecWebClient {
  val baseUrl = "/api/seqexec"

  def read(id: String): Future[Sequence] = {
    Ajax.get(
      url = s"$baseUrl/sequence/$id"
    ).map(s => default.read[Sequence](s.responseText))
  }

  def readQueue(): Future[SeqexecQueue] = {
    Ajax.get(
      url = s"$baseUrl/current/queue"
    ).map(s => default.read[SeqexecQueue](s.responseText))
  }
}
