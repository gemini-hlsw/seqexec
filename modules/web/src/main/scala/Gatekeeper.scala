// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import cats.data._
import cats.effect.IO
import cats.implicits._
import gem.{ Service => GemService }
import io.circe.generic.auto._
import java.time.Instant
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.server._

/**
 * A middleware that provides a login endpoint and implements JWT-based authorization, lifting
 * authenticated services that rely on a GemService[IO] (as our main app service does).
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
    def create(user: User[_], expiresIn: Long): IO[GemToken] =
      IO {
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
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    def encode(env: Environment, user: User[_]): IO[String] =
      create(user, env.config.jwt.ttlSeconds).map(env.encodeJwt(_))

    /**
     * Like `encode`, but embeds the encoded JWT in a cooke with environment-specified
     * name.
     */
    def cookie(env: Environment, user: User[_]): IO[Cookie] =
      encode(env, user).map(Cookie(env.config.jwt.cookieName, _))

    /**
     * Decode an encoded GemToken if possible, otherwise return an error string to be included
     * in the Forbidden() body.
     */
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    def decode(env: Environment, encoded: String): IO[Either[String, GemToken]] =
      env.decodeJwt[GemToken](encoded)

    /** Like `decode`, but pulls the encoded value from the envronment-defined cookie. */
    def decodeFromCookie(env: Environment, req: Request[IO]): IO[Either[String, GemToken]] =
      req.findCookie(env.config.jwt.cookieName) match {
        case Some(c) => GemToken.decode(env, c.content)
        case None    => IO.pure(Left(s"Cookie ${env.config.jwt.cookieName} not present."))
      }

  }

  // A data type for login requests.
  final case class LoginRequest(uid: String, pass: String)

  /** A service that listens to /login and issues new cookies. */
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference")) // false positive on decodeJson
  def login(env: Environment): HttpService[IO] =
    HttpService[IO] {
      // curl -i -d '{ "uid": "bobdole", "pass": "banana" }' localhost:8080/login
      case req @ POST -> Root / "login" =>
        req.decodeJson[LoginRequest].flatMap { case LoginRequest(u, p) =>
          env.tryLogin(u, p).flatMap {
            case None      => Forbidden("Login failed.")
            case Some(svc) => GemToken.cookie(env, svc.user).flatMap(c => Ok("Logged in.").map(_.addCookie(c)))
          }
        }
    }

  /**
   * Given an AuthedService and an Environment that knows how to authenticate user cookies, yield a
   * Normal HttpService that verifies the cookie on the way in and updates it on the way out. This
   * is a bit more complex than normal services because we must work in OptionT to handle the case
   * where `delegate` doesn't respond.
   */
  def authenticate(env: Environment, delegate: AuthedService[GemService[IO], IO]): HttpService[IO] =
    Kleisli[OptionT[IO, ?], Request[IO], Response[IO]] {
      // curl -i -b gem.jwt=... localhost:8080/something/else
      case req =>
        OptionT.liftF(GemToken.decodeFromCookie(env, req)).flatMap {
          case Left(msg)  => OptionT.liftF(Forbidden(msg))
          case Right(jwt) =>
            OptionT.liftF(env.service(jwt.uid)).flatMap {
              case None      => OptionT.liftF(Forbidden(s"JWT is valid but user ${jwt.uid} was not found."))
              case Some(svc) =>
                // Delegate and refresh our cookie as the response comes back.
                delegate.run(AuthedRequest(svc, req)).flatMap { res =>
                  OptionT.liftF(GemToken.cookie(env, svc.user).map(res.addCookie))
                }
            }
        }
    }

  /**
   * Construct the gatekeeper middleware. We need a server environment to figure out how to do
   * do this because we need a way to encode/decode tokens, as well as way to log users in.
   */
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  def apply(env: Environment): AuthMiddleware[IO, GemService[IO]] = delegate =>
    login(env) <+> authenticate(env, delegate)

}
