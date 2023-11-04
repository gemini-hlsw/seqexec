// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import cats.data.Nested
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.websocket.WebSocketBuilder2
import seqexec.model.UserLoginRequest
import seqexec.web.server.http4s.encoder._
import munit.CatsEffectSuite

class SeqexecUIApiRoutesSpec extends CatsEffectSuite with ClientBooEncoders with TestRoutes {
  test("SeqexecUIApiRoutes login: reject requests without body") {
    for {
      wsb <- WebSocketBuilder2[IO]
      s   <- uiRoutes(wsb)
      r   <- Nested(
               s.apply(Request(method = Method.POST, uri = uri"/seqexec/login")).value
             ).map(_.status).value
    } yield assertEquals(r, Some(Status.BadRequest))
  }

  test("SeqexecUIApiRoutes login: reject GET requests") {
    // This should in principle return a 405
    // see https://github.com/http4s/http4s/issues/234
    for {
      wsb <- WebSocketBuilder2[IO]
      s   <- uiRoutes(wsb)
      r   <- Nested(
               s.apply(Request(method = Method.GET, uri = uri"/seqexec/login")).value
             ).map(_.status).value
    } yield assertEquals(r, Some(Status.NotFound))
  }

  test("SeqexecUIApiRoutes login: successful login gives a cookie") {
    for {
      wsb <- WebSocketBuilder2[IO]
      s   <- uiRoutes(wsb)
      r   <- s
               .apply(
                 Request(method = Method.POST, uri = uri"/seqexec/login")
                   .withEntity(UserLoginRequest("telops", "pwd"))
               )
               .value
      s   <- r.map(_.status).pure[IO]
      k   <- r.map(_.cookies).orEmpty.pure[IO]
      t    = k.find(_.name === "token")
    } yield assert(t.isDefined && s === Some(Status.Ok))
  }

  test("SeqexecUIApiRoutes logout: successful logout clears the cookie") {
    for {
      wsb <- WebSocketBuilder2[IO]
      s   <- uiRoutes(wsb)
      t   <- newLoginToken(wsb)
      r   <- s(
               Request[IO](method = Method.POST, uri = uri"/seqexec/logout")
                 .addCookie("token", t)
             ).value
      s   <- r.map(_.status).pure[IO]
      k   <- r.map(_.cookies).orEmpty.pure[IO]
      t    = k.find(_.name === "token")
      c    = t.map(_.content).forall(_ === "") // Cleared cookie
    } yield assert(c && s === Some(Status.Ok))
  }

  test("SeqexecUIApiRoutes site") {
    for {
      wsb <- WebSocketBuilder2[IO]
      s   <- uiRoutes(wsb)
      t   <- newLoginToken(wsb)
      r   <- s(
               Request[IO](method = Method.POST, uri = uri"/seqexec/site")
                 .addCookie("token", t)
             ).value
      s   <- r.map(_.as[String]).sequence
    } yield assert(s === Some("GS"))
  }

}
