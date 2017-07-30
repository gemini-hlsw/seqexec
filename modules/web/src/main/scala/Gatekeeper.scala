// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import argonaut._, Argonaut._, ArgonautShapeless._
import gem.json._
import gem.{ Service => GemService }
import java.time.Instant
import org.http4s._
import org.http4s.dsl._
import org.http4s.server._
import scalaz.concurrent.Task

/**
 * A middleware that provides a login endpoint and implements JWT-based authorization, lifting
 * authenticated services that rely on a GemService[Task] (as our main app service does).
 */
object Gatekeeper {

  /**
   * Our JSON web token (JWT), for now containing only a user id and a span of time when it is
   * valid. It will be encoded and signed, yielding a token which is sent to the user on login, and
   * will be returned in subsequent requests. Note that some are used by JWT itself, so the field
   * names and their meaning is important in some cases. See https://jwt.io/
   */
  final case class GemToken(
    iat: Long,  // JWT spec: issued at,  in Epoch seconds
    exp: Long,  // JWT spec: expiration, in Epoch seconds
    uid: String // ours, not part of the spec
  )
  object GemToken {

    /** Construct a token, issued now and valid for `expiresIn` seconds. */
    def create(user: User[_], expiresIn: Long): Task[GemToken] =
      Task.delay {
        val now = Instant.now()
        new GemToken(
          iat = now.getEpochSecond,
          exp = now.plusSeconds(expiresIn).getEpochSecond,
          uid = user.id
        )
      }

    /**
     * Construct a token using the TTL defined in the envronment, and encode/sign it using the
     * algorithm and secret key also defined in the environment.
     */
    def encode(env: Environment, user: User[_]): Task[String] =
      create(user, env.config.jwt.ttlSeconds).map(env.encodeJwt(_))

    /**
     * Like `encode`, but embeds the encoded JWT in a cooke with environment-specified
     * name.
     */
    def cookie(env: Environment, user: User[_]): Task[Cookie] =
      encode(env, user).map(Cookie(env.config.jwt.cookieName, _))

    /**
     * Decode an encoded GemToken if possible, otherwise return an error string to be included
     * in the Forbidden() body.
     */
    def decode(env: Environment, encoded: String): Task[Either[String, GemToken]] =
      env.decodeJwt[GemToken](encoded)

    /** Like `decode`, but pulls the encoded value from the envronment-defined cookie. */
    def decodeFromCookie(env: Environment, req: Request): Task[Either[String, GemToken]] =
      req.findCookie(env.config.jwt.cookieName) match {
        case Some(c) => GemToken.decode(env, c.content)
        case None    => Task.now(Left(s"Cookie ${env.config.jwt.cookieName} not present."))
      }

  }

  // A data type for login requests.
  final case class LoginRequest(uid: String, pass: String)

  /**
   * Construct the gatekeeper middleware. We need a server environment to figure out how to do
   * do this because we need a way to encode/decode tokens, as well as way to log users in.
   */
  def apply(env: Environment): AuthMiddleware[GemService[Task]] = authed =>
    HttpService.lift {

      // curl -i -d '{ "uid": "bobdole", "pass": "banana" }' localhost:8080/login
      case req @ POST -> Root / "login" =>
        req.as[LoginRequest].flatMap { case LoginRequest(u, p) =>
          env.tryLogin(u, p).flatMap {
            case None      => Forbidden("Login failed.")
            case Some(svc) => GemToken.cookie(env, svc.user).flatMap(Ok("Logged in.").addCookie)
          }
        }

      // curl -i -b gem.jwt=... localhost:8080/something/else
      case req =>
        GemToken.decodeFromCookie(env, req).flatMap {
          case Left(msg)  => Forbidden(msg)
          case Right(jwt) =>
            env.service(jwt.uid).flatMap {
              case None      => Forbidden(s"JWT is valid but user ${jwt.uid} was not found.")
              case Some(svc) =>
                // Delegate to `authed` and refresh our cookie as the response comes back.
                authed.run(AuthedRequest(svc, req)).flatMap { res =>
                  res.cata(r => GemToken.cookie(env, svc.user).map(r.addCookie), Pass.now)
                }
            }
        }

    }

}
