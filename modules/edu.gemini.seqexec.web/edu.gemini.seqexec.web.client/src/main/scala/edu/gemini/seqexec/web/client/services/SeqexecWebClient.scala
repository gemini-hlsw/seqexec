package edu.gemini.seqexec.web.client.services

import java.util.logging.LogRecord

import edu.gemini.seqexec.model.{UserDetails, UserLoginRequest}
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.common.LogMessage._
import org.scalajs.dom.ext.{Ajax, AjaxException}
import upickle.default
import boopickle.Default._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer, Uint8Array}

/**
  * Encapsulates remote calls to the Seqexec Web API
  */
object SeqexecWebClient {
  val baseUrl = "/api/seqexec"

  def read(id: String): Future[List[Sequence]] =
    Ajax.get(
      url = s"$baseUrl/sequence/$id"
      //responseType = "arraybuffer"
    )
    .map(s => {
      /*println("a")
      val r = s.response.asInstanceOf[ArrayBuffer]
      println("b")
      println(r.byteLength)
      println(TypedArrayBuffer.wrap(r))
      println("c")*/
      try {
        //Unpickle[List[Sequence]].fromBytes(TypedArrayBuffer.wrap(s.response.asInstanceOf[ArrayBuffer]))
        default.read[List[Sequence]](s.responseText)
      } catch {
        case e: Exception =>
          e.printStackTrace()
          Nil
      }
    })
    .recover {
      case AjaxException(xhr) if xhr.status == HttpStatusCodes.NotFound  => Nil // If not found, we'll consider it like an empty response
    }

  def readQueue(): Future[SeqexecQueue] =
    Ajax.get(
      url = s"$baseUrl/current/queue"
    ).map(s => default.read[SeqexecQueue](s.responseText))

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
      data = default.write(UserLoginRequest(u, p))
    ).map(s => default.read[UserDetails](s.responseText))

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
      data = default.write(LogMessage.fromLogRecord(record))
    ).map(_.responseText)
}
