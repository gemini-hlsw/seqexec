// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats._
import cats.effect._
import cats.implicits._
import com.unboundid.ldap.sdk.LDAPURL
import io.chrisdavenport.log4cats.Logger
import io.circe._
import io.circe.syntax._
import io.circe.jawn.decode
import io.circe.generic.semiauto.deriveCodec
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim, JwtCirce}
import seqexec.model.UserDetails
import seqexec.model.config._
import seqexec.web.server.security.AuthenticationService.AuthResult

sealed trait AuthenticationFailure extends Product with Serializable
final case class UserNotFound(user: String) extends AuthenticationFailure
final case class BadCredentials(user: String) extends AuthenticationFailure
case object NoAuthenticator extends AuthenticationFailure
final case class GenericFailure(msg: String) extends AuthenticationFailure
final case class DecodingFailure(msg: String) extends AuthenticationFailure
case object MissingCookie extends AuthenticationFailure

/**
  * Interface for implementations that can authenticate users from a username/pwd pair
  */
trait AuthService[F[_]] {
  def authenticateUser(username: String, password: String): F[AuthResult]
}

// Intermediate class to decode the claim stored in the JWT token
final case class JwtUserClaim(exp: Int, iat: Int, username: String, displayName: String) {
  def toUserDetails: UserDetails = UserDetails(username, displayName)
}

final case class AuthenticationService[F[_]: Timer: Sync: Logger](mode: Mode, config: AuthenticationConfig) extends AuthService[F] {
  import AuthenticationService._
  implicit val clock = java.time.Clock.systemUTC()

  private val hosts = config.ldapURLs.map(u => new LDAPURL(u.renderString)).map(u => (u.getHost, u.getPort))

  val ldapService: AuthService[F] = new FreeLDAPAuthenticationService(hosts)

  implicit val codecForUserDetails: Codec[UserDetails] = deriveCodec

  private val authServices =
    if (mode === Mode.Development) List(new TestAuthenticationService[F], ldapService)
    else List(ldapService)

  /**
    * From the user details it creates a JSON Web Token
    */
  def buildToken(u: UserDetails): F[String] = Sync[F].delay {
    // Given that only this server will need the key we can just use HMAC. 512-bit is the max key size allowed
    Jwt.encode(JwtClaim(u.asJson.noSpaces).issuedNow.expiresIn(config.sessionLifeHrs.toSeconds.toLong), config.secretKey, JwtAlgorithm.HS512)
  }

  /**
    * Decodes a token out of JSON Web Token
    */
  def decodeToken(t: String): AuthResult =
    for {
      claim       <- JwtCirce.decode(t, config.secretKey, Seq(JwtAlgorithm.HS512)).toEither.leftMap(t => DecodingFailure(t.getMessage))
      userDetails <- decode[UserDetails](claim.content).leftMap(e => DecodingFailure(e.getMessage))
    } yield userDetails

  val sessionTimeout: Long = config.sessionLifeHrs.toSeconds

  override def authenticateUser(username: String, password: String): F[AuthResult] =
    authServices.authenticateUser(username, password)
}

object AuthenticationService {
  type AuthResult = Either[AuthenticationFailure, UserDetails]
  type AuthenticationServices[F[_]] = List[AuthService[F]]

  // Allows calling authenticate on a list of authenticator, stopping at the first
  // that succeeds
  implicit class ComposedAuth[F[_]: MonadError[?[_], Throwable]](val s: AuthenticationServices[F]) {

    def authenticateUser(username: String, password: String): F[AuthResult] = {
      def go(l: List[AuthService[F]]): F[AuthResult] = l match {
        case Nil      => NoAuthenticator.asLeft[UserDetails].pure[F].widen[AuthResult]
        case x :: Nil => x.authenticateUser(username, password)
        case x :: xs  => x.authenticateUser(username, password).attempt.flatMap {
            case Right(u) => u.pure[F]
            case Left(_)  => go(xs)
          }
      }
      // Discard empty values right away
      if (username.isEmpty || password.isEmpty) {
        BadCredentials(username).asLeft[UserDetails].pure[F].widen[AuthResult]
      } else {
        go(s)
      }
    }
  }
}
