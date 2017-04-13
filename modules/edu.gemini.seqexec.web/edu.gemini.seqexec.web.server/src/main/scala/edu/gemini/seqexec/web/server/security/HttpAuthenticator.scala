package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult

import org.http4s.{AuthedService, Request, Cookie}
import org.http4s.dsl._
import org.http4s.headers
import org.http4s.server.AuthMiddleware

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

import java.time.Instant

/**
  * Bridge between http4s authentication and the actual internal authenticator
  */
class Http4sAuthentication(auth: AuthenticationService) {
  val cookieService = CookiesService(auth.config.cookieName, auth.config.useSSL, auth.sessionTimeout.toSeconds.toLong)

  def loginCookie(user: UserDetails): Task[Cookie] = cookieService.loginCookie(auth, user)

  val authUser: Kleisli[Task, Request, AuthResult] = Kleisli({ request =>
    val authResult = for {
      header <- headers.Cookie.from(request.headers).toRightDisjunction(MissingCookie)
      cookie <- header.values.list.find(_.name === auth.config.cookieName).toRightDisjunction(MissingCookie)
      user   <- auth.decodeToken(cookie.content)
    } yield user
    Task.delay(authResult)
  })

  val optAuth = AuthMiddleware(authUser)

  val onFailure: AuthedService[AuthenticationFailure] = Kleisli(req => Forbidden())
  val reqAuth = AuthMiddleware(authUser, onFailure)

}

trait CookiesService {
  def name: String
  def ssl: Boolean
  def ttl: Long

  def buildCookie(token: String): Task[Cookie] = Task.delay {
    // if successful set a cookie
    val exp = Instant.now().plusSeconds(ttl)
    Cookie(name, token, path = "/".some, expires = exp.some, secure = ssl, httpOnly = true)
  }

  def loginCookie(auth: AuthenticationService, user: UserDetails): Task[Cookie] =
    auth.buildToken(user) >>= buildCookie
}

object CookiesService {
  def apply(cookieName: String, useSSL: Boolean, timeToLive: Long): CookiesService = new CookiesService {
    override val name = cookieName
    override val ssl = useSSL
    override val ttl = timeToLive
  }
}

