package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult
import upickle.default._
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim}

import scala.annotation.tailrec
import scalaz._
import Scalaz._
import scalaz.concurrent.Task

sealed trait AuthenticationFailure
case class UserNotFound(user: String) extends AuthenticationFailure
case class BadCredentials(user: String) extends AuthenticationFailure
case object NoAuthenticator extends AuthenticationFailure
case class GenericFailure(msg: String) extends AuthenticationFailure
case class DecodingFailure(msg: String) extends AuthenticationFailure
case object MissingCookie extends AuthenticationFailure

trait AuthService {
  def authenticateUser(username: String, password: String): AuthResult
}

case class LDAPConfig(ldapHosts: List[String], ldapPorts: List[Int]) {
  val ldapService = new FreeLDAPAuthenticationService(ldapHosts.headOption.getOrElse(""), ldapPorts.headOption.getOrElse(0))
}

case class AuthenticationConfig(devMode: Boolean, sessionLifeHrs: Int, cookieName: String, secretKey: String, useSSL: Boolean, ldap: LDAPConfig)

// Intermediate class to decode the claim stored in the JWT token
case class JwtUserClaim(exp: Int, iat: Int, username: String, displayName: String) {
  def toUserDetails = UserDetails(username, displayName)
}

case class AuthenticationService(config: AuthenticationConfig) extends AuthService {
  import AuthenticationService._

  val authServices =
    if (config.devMode) List(TestAuthenticationService, config.ldap.ldapService)
    else List(config.ldap.ldapService)

  /**
    * From the user details it creates a JSON Web Token
    */
  def buildToken(u: UserDetails): String =
    // Given that only this server will need the key we can just use HMAC. 512-bit is the max key size allowed
    Jwt.encode(JwtClaim(write(u)).issuedNow.expiresIn(3600), config.secretKey, JwtAlgorithm.HmacSHA256)

  /**
    * Decodes a token out of JSON Web Token
    */
  def decodeToken(t: String): AuthResult =
    (for {
      claim <- Jwt.decode(t, config.secretKey, Seq(JwtAlgorithm.HmacSHA256)).toDisjunction
      token <- \/.fromTryCatchNonFatal(read[JwtUserClaim](claim))
    } yield token.toUserDetails).leftMap(m => DecodingFailure(m.getMessage))

  val sessionTimeout: Long = config.sessionLifeHrs * 3600

  override def authenticateUser(username: String, password: String): AuthResult = authServices.authenticateUser(username, password)
}

object AuthenticationService {
  val Realm = "Seqexec"
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

  val authServices: Kleisli[Task, AuthenticationConfig, AuthenticationService] = Kleisli(c =>
    if (c.devMode) Task.now(AuthenticationService(c))
    else Task.now(AuthenticationService(c))
  )

}
