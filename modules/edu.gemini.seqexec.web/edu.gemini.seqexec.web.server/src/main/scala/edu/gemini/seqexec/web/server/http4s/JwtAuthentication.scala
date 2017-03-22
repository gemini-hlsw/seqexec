package edu.gemini.seqexec.web.server.http4s

import java.time.Instant

import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService
import org.http4s.{AttributeKey, Cookie, HttpService, Response}

import scalaz._
import Scalaz._
import scalaz.concurrent.Task


trait CookiesService {
  def name: String
  def ssl: Boolean

  def buildCookie(token: String, expiration: Long): Cookie = {
    // if successful set a cookie
    val exp = Instant.now().plusSeconds(expiration)
    Cookie(name, token, path = "/".some, expires = exp.some, secure = ssl, httpOnly = true)
  }

}

object CookiesService {
  def apply(cookieName: String, useSSL: Boolean): CookiesService = new CookiesService {
    override val name = cookieName
    override val ssl = useSSL
  }
}

object JwtAuthentication {
  val authenticatedUser: AttributeKey[Option[UserDetails]] = AttributeKey[Option[UserDetails]]("edu.gemini.seqexec.authenticatedUser")
}

case class JwtAuthentication(auth: AuthenticationService, override val optionalAllowed: Boolean) extends TokenAuthenticator[UserDetails] with TokenInCookies {
  override val cookieName = auth.config.cookieName
  override val attributeKey = JwtAuthentication.authenticatedUser
  override val store = (t: String) => auth.decodeToken(t) match {
    case \/-(u) => Task.now(Some(u))
    case -\/(_) => Task.now(None)
  }
  val cookieService = CookiesService(cookieName, auth.config.useSSL)
  def loginCookie(user: UserDetails): Cookie = {
    val cookieVal = auth.buildToken(user)
    cookieService.buildCookie(cookieVal, auth.sessionTimeout.toSeconds.toLong)
  }
  override def apply(service: HttpService): HttpService = super.apply(service).andThenK { (resp: Response) =>
    // If the user has the attribute replace the cookie
    Task.delay {
      resp.attributes.get(JwtAuthentication.authenticatedUser).flatten.fold(resp)(user => {
        resp.addCookie(loginCookie(user))
      })
    }
  }
}

