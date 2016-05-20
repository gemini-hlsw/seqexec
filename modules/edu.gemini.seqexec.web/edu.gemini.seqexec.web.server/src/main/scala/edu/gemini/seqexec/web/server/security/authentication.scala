package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.web.common.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult
import upickle.default._
import pdi.jwt.{Jwt, JwtAlgorithm, JwtClaim, JwtHeader, JwtOptions}

import scala.annotation.tailrec
import scala.util.Try
import scalaz.{-\/, \/, \/-}

sealed trait AuthenticationFailure
case class UserNotFound(user: String) extends AuthenticationFailure
case class BadCredentials(user: String) extends AuthenticationFailure
case object NoAuthenticator extends AuthenticationFailure
case class GenericFailure(msg: String) extends AuthenticationFailure

trait AuthenticationService {
  def authenticateUser(username: String, password: String): AuthResult
}

object AuthenticationConfig {
  // TODO externalize the configuration
  val sessionTimeout = 8 * 3600
  val onSSL = false
  val key = "secretkey"
  val cookieName = "SeqexecToken"

  val testMode = true
  val ldapHost = "gs-dc6.gemini.edu"
  val ldapPort = 3268

  val ldapService = new LDAPService(ldapHost, ldapPort)

  // TODO Only the LDAP service should be present on production mode
  val authServices = if (testMode) List(TestAuthenticationService, ldapService)
  else List(ldapService)

}

case class Token(exp: Int, iat: Int, username: String, displayName: String) {
  def toUserDetails = UserDetails(username, displayName)
}

object AuthenticationService {
  type AuthResult = AuthenticationFailure \/ UserDetails

  // Allows calling authenticate on a list of authenticator, stopping at the first
  // that succeeds
  implicit class ComposedAuth(val s: List[AuthenticationService]) extends AnyVal {

    def authenticateUser(username: String, password: String): AuthResult = {
      @tailrec
      def go(l: List[AuthenticationService]): AuthResult = l match {
        case Nil      => -\/(NoAuthenticator)
        case x :: Nil => x.authenticateUser(username, password)
        case x :: xs  => x.authenticateUser(username, password) match {
            case u @ \/-(_) => u
            case -\/(e)     => go(xs)
          }
      }
      go(s)
    }
  }

  def buildToken(u: UserDetails): String =
    // Given that only this server will need the key we can just use HMAC. 512-bit is the max key size allowed
    Jwt.encode(JwtClaim(write(u)).issuedNow.expiresIn(3600), AuthenticationConfig.key, JwtAlgorithm.HmacSHA256)

  def decodeToken(t: String): Try[UserDetails] = {
    for {
      claim <- Jwt.decode(t, AuthenticationConfig.key, Seq(JwtAlgorithm.HmacSHA256))
      token <- Try(read[Token](claim))
    } yield token.toUserDetails
  }
}