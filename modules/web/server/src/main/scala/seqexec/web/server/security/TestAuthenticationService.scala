// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats.Applicative
import cats.syntax.all._
import seqexec.model.UserDetails
import seqexec.web.server.security.AuthenticationService.AuthResult

/**
 * Authentication service for testing with a hardcoded list of users It lets you avoid the LDAP
 * dependency but should not be used in production
 */
class TestAuthenticationService[F[_]: Applicative] extends AuthService[F] {
  private val cannedUsers = List(UserDetails("telops", "Telops") -> "pwd")

  override def authenticateUser(username: String, password: String): F[AuthResult] =
    cannedUsers
      .collectFirst {
        case (ud @ UserDetails(u, _), p) if u === username && p === password => ud
      }
      .fold(BadCredentials(username).asLeft[UserDetails])(_.asRight)
      .pure[F]
      .widen[AuthResult]
}
