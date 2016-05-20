package edu.gemini.seqexec.web.client.services

import edu.gemini.seqexec.web.common._
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
}
