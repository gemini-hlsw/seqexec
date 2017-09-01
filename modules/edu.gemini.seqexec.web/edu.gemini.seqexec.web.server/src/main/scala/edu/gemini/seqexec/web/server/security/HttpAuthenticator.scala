// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult
import org.http4s._
import org.http4s.dsl._
import org.http4s.server.AuthMiddleware

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import java.time.Instant

/**
  * Bridge between http4s authentication and the actual internal authenticator
  */
class Http4sAuthentication(auth: AuthenticationService) {
  private val cookieService = CookiesService(auth.config.cookieName, auth.config.useSSL, auth.sessionTimeout.toSeconds.toLong)

  def loginCookie(user: UserDetails): Task[Cookie] = cookieService.loginCookie(auth, user)

  val authUser: Kleisli[Task, Request, AuthResult] = Kleisli({ request =>
    val authResult = for {
      header <- headers.Cookie.from(request.headers).toRightDisjunction(MissingCookie)
      cookie <- header.values.list.find(_.name === auth.config.cookieName).toRightDisjunction(MissingCookie)
      user   <- auth.decodeToken(cookie.content)
    } yield user
    Task.delay(authResult)
  })

  val optAuth: AuthMiddleware[AuthResult] = AuthMiddleware(authUser)

  private val onFailure: AuthedService[AuthenticationFailure] = Kleisli(req => Forbidden())
  val reqAuth: AuthMiddleware[UserDetails] = AuthMiddleware(authUser, onFailure)

}

/**
  * Middleware used to keep the session alive as long as authenticated request are comming
  * It has some cost as it needs to decode/encode the cookie with JWT
  */
object TokenRefresher {
  def apply(auth: Http4sAuthentication, service: HttpService): HttpService = auth.authUser.flatMap { result =>
    Service.lift { request: Request =>
       result.fold(_ => service(request), user =>
         auth.loginCookie(user) >>= { c =>
           service.map {
             case resp: Response =>
               // If auth is found replace the cookie
               resp.addCookie(c)
             case Pass => Pass
           }.apply(request)
         })
    }
  }
}

trait CookiesService {
  def name: String
  def ssl: Boolean
  def ttl: Long

  def buildCookie(token: String): Task[Cookie] =
    Task.delay {
      HttpDate.fromInstant(Instant.now().plusSeconds(ttl))
    }.map { exp => Cookie(name, token, path = "/".some, expires = exp.toOption, secure = ssl, httpOnly = true) }

  def loginCookie(auth: AuthenticationService, user: UserDetails): Task[Cookie] =
    auth.buildToken(user) >>= buildCookie
}

object CookiesService {
  def apply(cookieName: String, useSSL: Boolean, timeToLive: Long): CookiesService = new CookiesService {
    override val name: String = cookieName
    override val ssl: Boolean = useSSL
    override val ttl: Long = timeToLive
  }
}
