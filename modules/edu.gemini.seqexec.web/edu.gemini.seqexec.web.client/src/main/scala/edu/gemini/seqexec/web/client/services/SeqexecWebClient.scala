package edu.gemini.seqexec.web.client.services

import java.util.logging.LogRecord

import edu.gemini.seqexec.model.{UserDetails, UserLoginRequest}
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.common.LogMessage._
import org.scalajs.dom.ext.{Ajax, AjaxException}
import upickle.default
import boopickle.Default._
import boopickle.Pickler
import org.scalajs.dom.XMLHttpRequest

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer, Uint8Array}

/**
  * Encapsulates remote calls to the Seqexec Web API
  */
object SeqexecWebClient {
  val baseUrl = "/api/seqexec"

  // Decodes the binary response with BooPickle, errors are not handled
  def unpickle[A](r: XMLHttpRequest)(implicit u: Pickler[A]): A = {
    val ab = TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer])
    Unpickle[A].fromBytes(ab)
  }

  def read(id: String): Future[List[Sequence]] =
    Ajax.get(
      url = s"$baseUrl/sequence/$id",
      responseType = "arraybuffer"
    )
    .map(unpickle[List[Sequence]])
    .recover {
      case AjaxException(xhr) if xhr.status == HttpStatusCodes.NotFound  => Nil // If not found, we'll consider it like an empty response
    }

  def readQueue(): Future[SeqexecQueue] =
    Ajax.get(
      url = s"$baseUrl/current/queue",
      responseType = "arraybuffer"
    ).map(unpickle[SeqexecQueue])

  /**
    * Requests the backend to execute a sequence
    */
  def run(s: Sequence): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/${s.id}/run"
    ).map(s => default.read[RegularCommand](s.responseText))
  }

  /**
    * Requests the backend to stop a sequence
    */
  def stop(s: Sequence): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/${s.id}/stop"
    ).map(s => default.read[RegularCommand](s.responseText))
  }

  /**
    * Login request
    */
  def login(u: String, p: String): Future[UserDetails] =
    Ajax.post(
      url = s"$baseUrl/login",
      headers = Map("Content-Type" -> "application/octet-stream"),
      responseType = "arraybuffer",
      data = Pickle.intoBytes(UserLoginRequest(u, p))
    ).map(unpickle[UserDetails])

  /**
    * Logout request
    */
  def logout(): Future[String] =
    Ajax.post(
      url = s"$baseUrl/logout"
    ).map(_.responseText)

  /**
    * Log record
    */
  def log(record: LogRecord): Future[Unit] =
    Ajax.post(
      url = s"$baseUrl/log",
      responseType = "arraybuffer",
      data = Pickle.intoBytes(LogMessage.fromLogRecord(record))
    ).map(_.responseText)
}
