package edu.gemini.seqexec.web.server.security

import scalaz._
import Scalaz._

/**
  * Authentication service to avoid the LDAP dependency
  */
object TestAuthenticationService extends AuthenticationService {
  val cannedUsers = List(UserDetails("telops", "Telops") -> "pwd")

  override def authenticateUser(username: String, password: String): AuthenticationFailure \/ UserDetails = {
    cannedUsers.collect {
      case (ud @ UserDetails(u, _), p) if u == username && p == password => ud
    }.headOption.fold(BadCredentials(username).left[UserDetails])(_.right)
  }
}
