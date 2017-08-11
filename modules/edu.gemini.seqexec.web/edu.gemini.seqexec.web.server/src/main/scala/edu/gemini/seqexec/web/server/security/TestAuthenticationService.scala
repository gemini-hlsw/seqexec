// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.security

import edu.gemini.seqexec.model.UserDetails

import scalaz._
import Scalaz._

/**
  * Authentication service for testing with a hardcoded list of users
  * It lets you avoid the LDAP dependency but should not be used in production
  */
object TestAuthenticationService extends AuthService {
  val cannedUsers = List(UserDetails("telops", "Telops") -> "pwd")

  override def authenticateUser(username: String, password: String): AuthenticationFailure \/ UserDetails = {
    cannedUsers.collect {
      case (ud @ UserDetails(u, _), p) if u == username && p == password => ud
    }.headOption.fold(BadCredentials(username).left[UserDetails])(_.right)
  }
}
