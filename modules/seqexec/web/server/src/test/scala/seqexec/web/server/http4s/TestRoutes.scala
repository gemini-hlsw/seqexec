// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Timer
import cats.tests.CatsSuite
import fs2.concurrent.Queue
import fs2.concurrent.Topic
import giapi.client.GiapiStatusDb
import gem.enum.Site
import io.chrisdavenport.log4cats.noop.NoOpLogger
import org.http4s._
import org.http4s.Uri.uri
import seqexec.server._
import seqexec.server.tcs.GuideConfigDb
import seqexec.model.events._
import seqexec.model.config._
import seqexec.web.server.http4s.encoder._
import seqexec.web.server.security.AuthenticationService
import seqexec.model.UserLoginRequest
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait TestRoutes extends ClientBooEncoders with CatsSuite {
  private implicit def logger = NoOpLogger.impl[IO]

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  private val statusDb = GiapiStatusDb.simulatedDb[IO]
  private val config = AuthenticationConfig(FiniteDuration(8, HOURS),
                                            "token",
                                            "abc",
                                            useSSL = false,
                                            Nil)
  private val authService = AuthenticationService[IO](Mode.Development, config)

  def commandRoutes(engine: SeqexecEngine[IO]) =
    for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
    } yield new SeqexecCommandRoutes[IO](authService, q, engine).service

  val uiRoutes =
    for {
      o <- Topic[IO, SeqexecEvent](NullEvent)
    } yield
      new SeqexecUIApiRoutes(Site.GS,
                             Mode.Development,
                             authService,
                             GuideConfigDb.constant[IO],
                             statusDb,
                             o).service

  def newLoginToken: IO[String] =
    for {
      s <- uiRoutes
      r <- s
        .apply(
          Request(method = Method.POST, uri = uri("/seqexec/login"))
            .withEntity(UserLoginRequest("telops", "pwd"))
        )
        .value
      k <- r.map(_.cookies).orEmpty.find(_.name === "token").pure[IO]
    } yield k.map(_.content).orEmpty
}
