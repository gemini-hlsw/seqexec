package edu.gemini.web.server.common

import org.http4s.dsl._
import org.http4s.{HttpService, Uri}

import scalaz.concurrent.Task

case class RedirectToHttpsRoutes(toPort: Int, externalName: String) {
  val baseUri = Uri.fromString(s"https://$externalName:$toPort").getOrElse(uri("/"))

  val service = HttpService {
    case request => MovedPermanently(baseUri.withPath(request.uri.path))
  }
}
