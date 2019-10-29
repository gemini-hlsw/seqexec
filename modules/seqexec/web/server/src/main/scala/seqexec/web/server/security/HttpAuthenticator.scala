// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats._
import cats.data.{Kleisli, OptionT}
import cats.implicits._
import cats.effect.Sync
import seqexec.model.UserDetails
import seqexec.web.server.security.AuthenticationService.AuthResult
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.AuthMiddleware
import java.time.Instant

/**
  * Bridge between http4s authentication and the actual internal authenticator
  */
class Http4sAuthentication[F[_]: Sync](auth: AuthenticationService[F]) extends Http4sDsl[F] {
  private val cookieService =
    CookiesService(auth.config.cookieName, auth.config.useSSL, auth.sessionTimeout)

  def loginCookie(user: UserDetails): F[ResponseCookie] =
    cookieService.loginCookie(auth, user)

  private def authRequest(request: Request[F]) = {
    val authResult = for {
      header <- headers.Cookie.from(request.headers).toRight(MissingCookie)
      cookie <- header.values.toList.find(_.name === auth.config.cookieName).toRight(MissingCookie)
      user   <- auth.decodeToken(cookie.content)
    } yield user
    authResult
  }

  val authUser: Kleisli[F, Request[F], AuthResult] = Kleisli( request =>
    Sync[F].delay(authRequest(request))
  )

  val optAuthUser: Kleisli[OptionT[F, ?], Request[F], AuthResult] = Kleisli( request =>
    OptionT.liftF(Sync[F].delay(authRequest(request)))
  )

  val optAuth: AuthMiddleware[F, AuthResult] = AuthMiddleware(optAuthUser)

  private val onFailure: AuthedRoutes[AuthenticationFailure, F] = Kleisli(_ => OptionT.liftF(Forbidden()))
  val reqAuth: AuthMiddleware[F, UserDetails] = AuthMiddleware(authUser, onFailure)

}

/**
  * Middleware used to keep the session alive as long as authenticated request are comming
  * It has some cost as it needs to decode/encode the cookie with JWT
  */
object TokenRefresher {
  private def replaceCookie[F[_]: Monad](service: HttpRoutes[F], auth: Http4sAuthentication[F])(result: AuthResult): Kleisli[OptionT[F, ?], Request[F], Response[F]] = Kleisli { request =>
    result.fold(_ => service(request), u =>
      OptionT.liftF(auth.loginCookie(u)) >>= { c => service.map(_.addCookie(c)).apply(request) })
  }

  def apply[F[_]: Monad](service: HttpRoutes[F], auth: Http4sAuthentication[F]): HttpRoutes[F] = auth.optAuthUser.flatMap(replaceCookie[F](service, auth))
}

trait CookiesService {
  def name: String
  def ssl: Boolean
  def ttl: Long

  def buildCookie[F[_]: Sync](token: String): F[ResponseCookie] =
    Sync[F].delay {
      HttpDate.fromInstant(Instant.now().plusSeconds(ttl))
    }.map { exp => ResponseCookie(name, token, path = "/".some, expires = exp.toOption, secure = ssl, httpOnly = true) }

  def loginCookie[F[_]: Sync](auth: AuthenticationService[F], user: UserDetails): F[ResponseCookie] =
    auth.buildToken(user) >>= buildCookie[F]
}

object CookiesService {
  def apply(cookieName: String, useSSL: Boolean, timeToLive: Long): CookiesService = new CookiesService {
    override val name: String = cookieName
    override val ssl: Boolean = useSSL
    override val ttl: Long = timeToLive
  }
}
