// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import seqexec.model.UserDetails
import seqexec.web.server.security.AuthenticationService.AuthResult
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.AuthMiddleware
import java.time.Instant

import cats.data.{Kleisli, OptionT}
import cats.implicits._
import cats.effect.IO

/**
  * Bridge between http4s authentication and the actual internal authenticator
  */
class Http4sAuthentication(auth: AuthenticationService) {
  private val cookieService = CookiesService(auth.config.cookieName, auth.config.useSSL, auth.sessionTimeout.toSeconds.toLong)

  def loginCookie(user: UserDetails): IO[Cookie] = cookieService.loginCookie(auth, user)

  private def authRequest(request: Request[IO]) = {
    val authResult = for {
      header <- headers.Cookie.from(request.headers).toRight(MissingCookie)
      cookie <- header.values.toList.find(_.name === auth.config.cookieName).toRight(MissingCookie)
      user   <- auth.decodeToken(cookie.content)
    } yield user
    authResult
  }

  val authUser: Kleisli[IO, Request[IO], AuthResult] = Kleisli( request =>
    IO(authRequest(request))
  )

  val optAuthUser: Kleisli[OptionT[IO, ?], Request[IO], AuthResult] = Kleisli( request =>
    OptionT.liftF(IO(authRequest(request)))
  )

  val optAuth: AuthMiddleware[IO, AuthResult] = AuthMiddleware(optAuthUser)

  private val onFailure: AuthedService[AuthenticationFailure, IO] = Kleisli(_ => OptionT.liftF(Forbidden()))
  val reqAuth: AuthMiddleware[IO, UserDetails] = AuthMiddleware(authUser, onFailure)

}

/**
  * Middleware used to keep the session alive as long as authenticated request are comming
  * It has some cost as it needs to decode/encode the cookie with JWT
  */
object TokenRefresher {
  private def replaceCookie(service: HttpService[IO], auth: Http4sAuthentication)(result: AuthResult): Kleisli[OptionT[IO, ?], Request[IO], Response[IO]] = Kleisli { request =>
    result.fold(_ => service(request), u =>
      OptionT.liftF(auth.loginCookie(u)) >>= { c => service.map(_.addCookie(c)).apply(request) })
  }

  def apply(service: HttpService[IO], auth: Http4sAuthentication): HttpService[IO] = auth.optAuthUser.flatMap(replaceCookie(service, auth))
}

trait CookiesService {
  def name: String
  def ssl: Boolean
  def ttl: Long

  def buildCookie(token: String): IO[Cookie] =
    IO.apply {
      HttpDate.fromInstant(Instant.now().plusSeconds(ttl))
    }.map { exp => Cookie(name, token, path = "/".some, expires = exp.toOption, secure = ssl, httpOnly = true) }

  def loginCookie(auth: AuthenticationService, user: UserDetails): IO[Cookie] =
    auth.buildToken(user) >>= buildCookie
}

object CookiesService {
  def apply(cookieName: String, useSSL: Boolean, timeToLive: Long): CookiesService = new CookiesService {
    override val name: String = cookieName
    override val ssl: Boolean = useSSL
    override val ttl: Long = timeToLive
  }
}
