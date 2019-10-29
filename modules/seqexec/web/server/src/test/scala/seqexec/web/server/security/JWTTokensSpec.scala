// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats.effect.IO
import cats.effect.Timer
import cats.tests.CatsSuite
import io.chrisdavenport.log4cats.noop.NoOpLogger
import seqexec.model.config._
import seqexec.model.UserDetails
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

class JWTTokensSpec extends CatsSuite {
  private implicit def logger = NoOpLogger.impl[IO]
  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  private val config = AuthenticationConfig(FiniteDuration(8, HOURS), "token", "key", useSSL = false, Nil)
  private val authService = AuthenticationService[IO](Mode.Production, config)

  test("JWT Tokens: encode/decode") {
    forAll { (u: String, p: String) =>
      val userDetails = UserDetails(u, p)
      val token = authService.buildToken(userDetails).unsafeRunSync
      Right(userDetails) shouldEqual authService.decodeToken(token)
    }
  }
}
