// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.tests.CatsSuite
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.model.UserDetails
import seqexec.model.config._

import scala.concurrent.duration._

class JWTTokensSpec extends CatsSuite {
  private implicit def logger = NoOpLogger.impl[IO]

  private val config      =
    AuthenticationConfig(FiniteDuration(8, HOURS), "token", "key", useSSL = false, Nil)
  private val authService = AuthenticationService[IO](Mode.Production, config)

  test("JWT Tokens: encode/decode") {
    forAll { (u: String, p: String) =>
      val userDetails = UserDetails(u, p)
      val token       = authService.buildToken(userDetails).unsafeRunSync()
      Right(userDetails) shouldEqual authService.decodeToken(token)
    }
  }
}
