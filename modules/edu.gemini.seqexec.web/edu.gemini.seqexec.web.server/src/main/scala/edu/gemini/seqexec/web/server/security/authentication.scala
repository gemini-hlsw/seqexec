package edu.gemini.seqexec.web.server.security

import com.unboundid.ldap.sdk.LDAPURL
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim}
import argonaut._, Argonaut._
import squants.Time
import squants.time.Seconds

import scala.annotation.tailrec
import scalaz._
import Scalaz._

sealed trait AuthenticationFailure
case class UserNotFound(user: String) extends AuthenticationFailure
case class BadCredentials(user: String) extends AuthenticationFailure
case object NoAuthenticator extends AuthenticationFailure
case class GenericFailure(msg: String) extends AuthenticationFailure
case class DecodingFailure(msg: String) extends AuthenticationFailure
case object MissingCookie extends AuthenticationFailure

/**
  * Interface for implementations that can authenticate users from a username/pwd pair
  */
trait AuthService {
  def authenticateUser(username: String, password: String): AuthResult
}

/**
  * Configuration for the LDAP client
  */
case class LDAPConfig(ldapHosts: List[String]) {
  val hosts = ldapHosts.map(new LDAPURL(_)).map(u => (u.getHost, u.getPort))

  val ldapService = new FreeLDAPAuthenticationService(hosts)
}

/**
  * Configuration for the general authentication service
  * @param devMode Indicates if we are in development mode, In this mode there is an internal list of users
  * @param sessionLifeHrs How long will the session live in hours
  * @param cookieName Name of the cookie to store the token
  * @param secretKey Secret key to encrypt jwt tokens
  * @param useSSL Whether we use SSL setting the cookie to be https only
  * @param ldap Configuration for the ldap client
  */
case class AuthenticationConfig(devMode: Boolean, sessionLifeHrs: Time, cookieName: String, secretKey: String, useSSL: Boolean, ldap: LDAPConfig)

// Intermediate class to decode the claim stored in the JWT token
case class JwtUserClaim(exp: Int, iat: Int, username: String, displayName: String) {
  def toUserDetails = UserDetails(username, displayName)
}

case class AuthenticationService(config: AuthenticationConfig) extends AuthService {
  import AuthenticationService._

  implicit def UserDetailsCodecJson: CodecJson[UserDetails] =
    casecodec2(UserDetails.apply, UserDetails.unapply)("username", "displayName")

  val authServices =
    if (config.devMode) List(TestAuthenticationService, config.ldap.ldapService)
    else List(config.ldap.ldapService)

  /**
    * From the user details it creates a JSON Web Token
    */
  def buildToken(u: UserDetails): String =
    // Given that only this server will need the key we can just use HMAC. 512-bit is the max key size allowed
    Jwt.encode(JwtClaim(u.asJson.nospaces).issuedNow.expiresIn(30), config.secretKey, JwtAlgorithm.HS512)

  /**
    * Decodes a token out of JSON Web Token
    */
  def decodeToken(t: String): AuthResult =
    for {
      claim       <- Jwt.decodeRaw(t, config.secretKey, Seq(JwtAlgorithm.HS512)).toDisjunction.leftMap(t => DecodingFailure(t.getMessage))
      userDetails <- \/.fromEither(claim.decodeEither[UserDetails].leftMap(DecodingFailure.apply))
    } yield userDetails

  val sessionTimeout: Time = config.sessionLifeHrs in Seconds

  override def authenticateUser(username: String, password: String): AuthResult =
    authServices.authenticateUser(username, password)
}

object AuthenticationService {
  type AuthResult = AuthenticationFailure \/ UserDetails
  type AuthenticationServices = List[AuthService]

  // Allows calling authenticate on a list of authenticator, stopping at the first
  // that succeeds
  implicit class ComposedAuth(val s: AuthenticationServices) extends AnyVal {

    def authenticateUser(username: String, password: String): AuthResult = {
      @tailrec
      def go(l: List[AuthService]): AuthResult = l match {
        case Nil      => -\/(NoAuthenticator)
        case x :: Nil => x.authenticateUser(username, password)
        case x :: xs  => x.authenticateUser(username, password) match {
            case u @ \/-(_) => u
            case -\/(e)     => go(xs)
          }
      }
      // Discard empty values right away
      if (username.isEmpty || password.isEmpty) {
        \/.left(BadCredentials(username))
      } else {
        go(s)
      }
    }
  }
}
