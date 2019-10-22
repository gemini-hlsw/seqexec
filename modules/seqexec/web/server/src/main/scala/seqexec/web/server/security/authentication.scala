// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import argonaut._
import Argonaut._
import cats.effect.IO
import cats.implicits._
import com.unboundid.ldap.sdk.LDAPURL
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim}
import seqexec.model.UserDetails
import seqexec.model.config._
import seqexec.web.server.security.AuthenticationService.AuthResult

sealed trait AuthenticationFailure
final case class UserNotFound(user: String) extends AuthenticationFailure
final case class BadCredentials(user: String) extends AuthenticationFailure
case object NoAuthenticator extends AuthenticationFailure
final case class GenericFailure(msg: String) extends AuthenticationFailure
final case class DecodingFailure(msg: String) extends AuthenticationFailure
case object MissingCookie extends AuthenticationFailure

/**
  * Interface for implementations that can authenticate users from a username/pwd pair
  */
trait AuthService {
  def authenticateUser(username: String, password: String): IO[AuthResult]
}

// Intermediate class to decode the claim stored in the JWT token
final case class JwtUserClaim(exp: Int, iat: Int, username: String, displayName: String) {
  def toUserDetails: UserDetails = UserDetails(username, displayName)
}

final case class AuthenticationService(mode: Mode, config: AuthenticationConfig) extends AuthService {
  import AuthenticationService._

  private val hosts = config.ldapURLs.map(u => new LDAPURL(u.renderString)).map(u => (u.getHost, u.getPort))

  val ldapService: AuthService = new FreeLDAPAuthenticationService(hosts)

  implicit def UserDetailsCodecJson: CodecJson[UserDetails] =
    casecodec2(UserDetails.apply, UserDetails.unapply)("username", "displayName")

  private val authServices =
    if (mode === Mode.Development) List(TestAuthenticationService, ldapService)
    else List(ldapService)

  /**
    * From the user details it creates a JSON Web Token
    */
  def buildToken(u: UserDetails): IO[String] = IO.apply {
    // Given that only this server will need the key we can just use HMAC. 512-bit is the max key size allowed
    Jwt.encode(JwtClaim(u.asJson.nospaces).issuedNow.expiresIn(config.sessionLifeHrs.toSeconds.toLong), config.secretKey, JwtAlgorithm.HS512)
  }

  /**
    * Decodes a token out of JSON Web Token
    */
  def decodeToken(t: String): AuthResult =
    for {
      claim       <- Jwt.decodeRaw(t, config.secretKey, Seq(JwtAlgorithm.HS512)).toEither.leftMap(t => DecodingFailure(t.getMessage))
      userDetails <- claim.decodeEither[UserDetails].leftMap(DecodingFailure.apply)
    } yield userDetails

  val sessionTimeout: Long = config.sessionLifeHrs.toSeconds

  override def authenticateUser(username: String, password: String): IO[AuthResult] =
    authServices.authenticateUser(username, password)
}

object AuthenticationService {
  type AuthResult = Either[AuthenticationFailure, UserDetails]
  type AuthenticationServices = List[AuthService]

  // Allows calling authenticate on a list of authenticator, stopping at the first
  // that succeeds
  implicit class ComposedAuth(val s: AuthenticationServices) extends AnyVal {

    def authenticateUser(username: String, password: String): IO[AuthResult] = {
      def go(l: List[AuthService]): IO[AuthResult] = l match {
        case Nil      => IO(Left(NoAuthenticator))
        case x :: Nil => x.authenticateUser(username, password)
        case x :: xs  => x.authenticateUser(username, password).attempt.flatMap {
            case Right(u) => IO.pure(u)
            case Left(_)      => go(xs)
          }
      }
      // Discard empty values right away
      if (username.isEmpty || password.isEmpty) {
        IO.pure(Left(BadCredentials(username)))
      } else {
        go(s)
      }
    }
  }
}
