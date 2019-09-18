// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import seqexec.model.UserDetails
import squants.time.Hours
import cats.tests.CatsSuite

class JWTTokensSpec extends CatsSuite {
  private val ldapConfig = LDAPConfig(Nil)
  private val config = AuthenticationConfig(devMode = true, Hours(8), "token", "key", useSSL = false, ldapConfig)
  private val authService = AuthenticationService(config)

  test("JWT Tokens: encode/decode") {
    forAll { (u: String, p: String) =>
      val userDetails = UserDetails(u, p)
      val token = authService.buildToken(userDetails).unsafeRunSync
      Right(userDetails) shouldEqual authService.decodeToken(token)
    }
  }
}
