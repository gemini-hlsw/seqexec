package edu.gemini.seqexec.web.server.http4s

import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService
import org.http4s.AttributeKey

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object JwtAuthentication {
  val authenticatedUser = AttributeKey[Option[UserDetails]]("edu.gemini.seqexec.authenticatedUser")
}

case class JwtAuthentication(auth: AuthenticationService) extends TokenAuthenticator[UserDetails] with TokenInCookies {
  override val cookieName = auth.cookieName
  override val attributeKey = JwtAuthentication.authenticatedUser
  override val store = (t: String) => auth.decodeToken(t) match {
    case \/-(u) => Task.now(Some(u))
    case -\/(e) => Task.now(None)
  }
}

