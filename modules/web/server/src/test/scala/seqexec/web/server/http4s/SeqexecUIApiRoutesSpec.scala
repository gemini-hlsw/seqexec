// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import cats.data.Nested
import cats.tests.CatsSuite
import org.http4s._
import org.http4s.Uri.uri
import seqexec.model.UserLoginRequest
import seqexec.web.server.http4s.encoder._

class SeqexecUIApiRoutesSpec extends CatsSuite with ClientBooEncoders with TestRoutes {
  test("SeqexecUIApiRoutes login: reject requests without body") {
    (for {
      s <- uiRoutes
      r <- Nested(
             s.apply(Request(method = Method.POST, uri = uri("/seqexec/login"))).value
           ).map(_.status).value
    } yield assert(r === Some(Status.BadRequest))).unsafeRunSync()
  }

  test("SeqexecUIApiRoutes login: reject GET requests") {
    // This should in principle return a 405
    // see https://github.com/http4s/http4s/issues/234
    val r = (for {
      s <- uiRoutes
      r <- Nested(
             s.apply(Request(method = Method.GET, uri = uri("/seqexec/login"))).value
           ).map(_.status).value
    } yield r).unsafeRunSync()
    assert(r === Some(Status.NotFound))
  }

  test("SeqexecUIApiRoutes login: successful login gives a cookie") {
    (for {
      s <- uiRoutes
      r <- s
             .apply(
               Request(method = Method.POST, uri = uri("/seqexec/login"))
                 .withEntity(UserLoginRequest("telops", "pwd"))
             )
             .value
      s <- r.map(_.status).pure[IO]
      k <- r.map(_.cookies).orEmpty.pure[IO]
      t  = k.find(_.name === "token")
    } yield assert(t.isDefined && s === Some(Status.Ok))).unsafeRunSync()
  }

  test("SeqexecUIApiRoutes logout: successful logout clears the cookie") {
    (for {
      s <- uiRoutes
      t <- newLoginToken
      r <- s(
             Request[IO](method = Method.POST, uri = uri("/seqexec/logout"))
               .addCookie("token", t)
           ).value
      s <- r.map(_.status).pure[IO]
      k <- r.map(_.cookies).orEmpty.pure[IO]
      t  = k.find(_.name === "token")
      c  = t.map(_.content).exists(_ === "") // Cleared cookie
    } yield assert(c && s === Some(Status.Ok))).unsafeRunSync()
  }

  test("SeqexecUIApiRoutes site") {
    (for {
      s <- uiRoutes
      t <- newLoginToken
      r <- s(
             Request[IO](method = Method.POST, uri = uri("/seqexec/site"))
               .addCookie("token", t)
           ).value
      s <- r.map(_.as[String]).sequence
    } yield assert(s === Some("GS"))).unsafeRunSync()
  }

}
