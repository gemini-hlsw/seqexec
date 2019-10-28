// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.{ ContextShift, IO, Timer }
import cats.tests.CatsSuite
import fs2.concurrent.Topic
import fs2.Stream
import gem.enum.Site
import giapi.client.GiapiStatusDb
import org.http4s._
import org.http4s.Uri.uri
import seqexec.model.events._
import seqexec.server.tcs.GuideConfigDb
import seqexec.model.config._
import seqexec.web.server.security.AuthenticationService
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class SeqexecUIApiRoutesSpec extends CatsSuite {

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  val statusDb = GiapiStatusDb.simulatedDb[IO]

  private val config = AuthenticationConfig(FiniteDuration(8, HOURS), "token", "abc", useSSL = false, Nil)
  private val authService = AuthenticationService(Mode.Production, config)
  val out: Stream[IO, Topic[IO, SeqexecEvent]] = Stream.eval(Topic[IO, SeqexecEvent](NullEvent))

  private val service =
    for {
      o <- out
    } yield new SeqexecUIApiRoutes(Site.GS, Mode.Development, authService, GuideConfigDb.constant[IO], statusDb, o).service

  test("SeqexecUIApiRoutes login: reject requests without body") {
    for {
      s <- service
    } yield {
      s.apply(Request(method = Method.POST, uri = uri("/seqexec/login"))).value.unsafeRunSync.map(_.status) should contain(Status.FailedDependency)
    }
  }

  test("SeqexecUIApiRoutes login: reject GET requests") {
    // This should in principle return a 405
    // see https://github.com/http4s/http4s/issues/234
    for {
      s <- service
    } yield s.apply(Request(uri = uri("/seqexec/login"))).value.unsafeRunSync.map(_.status) should contain(Status.NotFound)
  }

}
