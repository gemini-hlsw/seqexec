// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.server.common

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.ContextShift
import cats.effect.Timer
import cats.effect.Blocker
import org.http4s.CacheDirective.`max-age`
import org.http4s.MediaType._
import org.http4s._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s.Uri.uri
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext // N.B. using global for our blocking context here

class StaticRoutesSpec extends AnyFlatSpec with Matchers with EitherValues {

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val ioTimer: Timer[IO] =
    IO.timer(ExecutionContext.global)

  private val builtAtMillis = 1000L

  def index(devMode: Boolean): String = {
    val deps = if (devMode) "package-jsdeps.js" else s"package-jsdeps.min.$builtAtMillis.js"
    val script = if (devMode) s"app.js" else s"app-opt.$builtAtMillis.js"

    val xml =
      <html lang="en">
        <head>
          <meta charset="utf-8"/>
          <meta http-equiv="X-UA-Compatible" content="IE=edge"/>

          <title>Title</title>

          <link rel="stylesheet" href={s"css/semantic.$builtAtMillis.css"}/>
        </head>

        <body>

          <div id="content">
          </div>

          <script src={deps}></script>
          <script src={script}></script>
        </body>
      </html>
    s"<!DOCTYPE html>$xml"
  }

  "StaticRoutes regular files" should
    "return a file if present on resources" in {
      val service = Blocker[IO].use(b => IO(new StaticRoutes[IO](false, builtAtMillis, b).service))
      service.flatMap(_.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.status))).unsafeRunSync should contain(Status.Ok)
      service.flatMap(_.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers))).unsafeRunSync.getOrElse(Headers.empty).toList should contain (`Content-Type`(text.css))
    }
    it should "not leak the application configuration file" in {
      val service = Blocker[IO].use(b => IO(new StaticRoutes[IO](true, builtAtMillis, b).service))
      service.flatMap(_.apply(Request(uri = uri("/app.conf"))).value.map(_.map(_.status))).unsafeRunSync shouldBe empty
    }
    it should "cache them for a year on production mode" in {
      val service = Blocker[IO].use(b => IO(new StaticRoutes[IO](false, builtAtMillis, b).service))
      service.flatMap(_.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers))).unsafeRunSync.getOrElse(Headers.empty).toList should contain (`Cache-Control`(NonEmptyList.of(`max-age`(31536000.seconds))))
    }
    it should "not cache them on dev mode" in {
      val service = Blocker[IO].use(b => IO(new StaticRoutes[IO](true, builtAtMillis, b).service))
      service.flatMap(_.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers))).unsafeRunSync.getOrElse(Headers.empty).toList should not contain `Cache-Control`(NonEmptyList.of(`max-age`(31536000.seconds)))
    }
    it should "support fingerprinting" in {
      val service = Blocker[IO].use(b => IO(new StaticRoutes[IO](true, builtAtMillis, b).service))
      service.flatMap(_.apply(Request(uri = Uri.fromString(s"/css/test.$builtAtMillis.css").toOption.fold(uri("/"))(x => x))).value.map(_.map(_.status))).unsafeRunSync() should contain(Status.Ok)
    }
}
