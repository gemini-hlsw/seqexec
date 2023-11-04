// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import cats.tests.CatsSuite
import cats.effect.std.Queue
import fs2.concurrent.Topic
import giapi.client.GiapiStatusDb
import lucuma.core.enums.Site
import org.typelevel.log4cats.noop.NoOpLogger
import org.http4s._
import org.http4s.implicits._
import seqexec.server._
import seqexec.server.tcs.GuideConfigDb
import seqexec.model.events._
import seqexec.model.config._
import seqexec.web.server.http4s.encoder._
import seqexec.web.server.security.AuthenticationService
import seqexec.model.UserLoginRequest
import scala.concurrent.duration._
import cats.effect.Ref
import org.http4s.server.websocket.WebSocketBuilder2

trait TestRoutes extends ClientBooEncoders with CatsSuite {
  private implicit def logger = NoOpLogger.impl[IO]

  private val statusDb    = GiapiStatusDb.simulatedDb[IO]
  private val config      =
    AuthenticationConfig(FiniteDuration(8, HOURS), "token", "abc", useSSL = false, Nil)
  private val authService = AuthenticationService[IO](Mode.Development, config)

  def commandRoutes(engine: SeqexecEngine[IO]) =
    for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
    } yield new SeqexecCommandRoutes[IO](authService, q, engine).service

  def uiRoutes(wsb: WebSocketBuilder2[IO]) =
    for {
      o  <- Topic[IO, SeqexecEvent]
      cs <- Ref.of[IO, ClientsSetDb.ClientsSet](Map.empty).map(ClientsSetDb.apply[IO](_))
    } yield new SeqexecUIApiRoutes(Site.GS,
                                   Mode.Development,
                                   authService,
                                   GuideConfigDb.constant[IO],
                                   statusDb,
                                   cs,
                                   o,
                                   wsb
    ).service

  def newLoginToken(wsb: WebSocketBuilder2[IO]): IO[String] =
    for {
      s <- uiRoutes(wsb)
      r <- s
             .apply(
               Request(method = Method.POST, uri = uri"/seqexec/login")
                 .withEntity(UserLoginRequest("telops", "pwd"))
             )
             .value
      k <- r.map(_.cookies).orEmpty.find(_.name === "token").pure[IO]
    } yield k.map(_.content).orEmpty
}
