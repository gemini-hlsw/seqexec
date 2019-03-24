// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.server.common

import cats.data.NonEmptyList
import org.http4s.CacheDirective.`max-age`
import org.http4s.MediaType._
import org.http4s._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s.Uri.uri
import org.scalatest.{EitherValues, FlatSpec, Matchers}

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext // N.B. using global for our blocking context here

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw"))
class StaticRoutesSpec extends FlatSpec with Matchers with EitherValues {
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
      val service = new StaticRoutes(false, builtAtMillis, ExecutionContext.global).service
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.status)).unsafeRunSync should contain(Status.Ok)
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Content-Type`(text.css))
    }
    it should "not leak the application configuration file" in {
      val service = new StaticRoutes(true, builtAtMillis, ExecutionContext.global).service
      service.apply(Request(uri = uri("/app.conf"))).value.map(_.map(_.status)).unsafeRunSync shouldBe empty
    }
    it should "cache them for a year on production mode" in {
      val service = new StaticRoutes(false, builtAtMillis, ExecutionContext.global).service
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Cache-Control`(NonEmptyList.of(`max-age`(31536000.seconds))))
    }
    it should "not cache them on dev mode" in {
      val service = new StaticRoutes(true, builtAtMillis, ExecutionContext.global).service
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should not contain `Cache-Control`(NonEmptyList.of(`max-age`(31536000.seconds)))
    }
    it should "support fingerprinting" in {
      val service = new StaticRoutes(true, builtAtMillis, ExecutionContext.global).service
      service.apply(Request(uri = Uri.fromString(s"/css/test.$builtAtMillis.css").toOption.fold(uri("/"))(x => x))).value.map(_.map(_.status)).unsafeRunSync() should contain(Status.Ok)
    }
}
