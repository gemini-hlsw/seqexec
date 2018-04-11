// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.common.http4s

import cats.data.NonEmptyList
import edu.gemini.web.server.common.StaticRoutes
import fs2.text
import org.http4s.CacheDirective.{`max-age`, `no-cache`}
import org.http4s.MediaType._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s._
import org.scalatest.{EitherValues, FlatSpec, Matchers}
import cats.implicits._
import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw"))
class StaticRoutesSpec extends FlatSpec with Matchers with EitherValues with UriFunctions {
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

  "StaticRoutes index(true)" should
    "return a generated index(true) with production files on production mode" in {
      val service = new StaticRoutes(index(false), false, builtAtMillis).service
      service.apply(Request(uri = uri("/"))).value.map(_.map(_.status)).unsafeRunSync should contain(Status.Ok)
      service.apply(Request(uri = uri("/"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Cache-Control`(NonEmptyList.of(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).value.map(_.map(_.body.through(text.utf8Decode).compile.foldMonoid)).unsafeRunSync.map(_.unsafeRunSync())
      body.getOrElse("") should include regex ".*jsdeps\\.min\\.(\\d*)\\.js.*"
      body.getOrElse("") should include regex ".*app-opt\\.(\\d*)\\.js.*"
    }
    it should "return a generated index(true) with development files on dev mode" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/"))).value.map(_.map(_.status)).unsafeRunSync should contain(Status.Ok)
      service.apply(Request(uri = uri("/"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Cache-Control`(NonEmptyList.of(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).value.map(_.map(_.body.through(text.utf8Decode).compile.foldMonoid)).unsafeRunSync.map(_.unsafeRunSync())
      body.getOrElse("") should include regex ".*jsdeps\\.js.*"
      body.getOrElse("") should include regex ".*app*\\.js.*"
    }

  "StaticRoutes regular files" should
    "return a file if present on resources" in {
      val service = new StaticRoutes(index(false), false, builtAtMillis).service
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.status)).unsafeRunSync should contain(Status.Ok)
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Content-Type`(`text/css`))
    }
    it should "return the index for an unknown file" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/unknown"))).value.map(_.map(_.status)).unsafeRunSync should contain(Status.Ok)
    }
    it should "not leak the application configuration file" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/app.conf"))).value.map(_.map(_.status)).unsafeRunSync shouldBe empty
    }
    it should "cache them for a year on production mode" in {
      val service = new StaticRoutes(index(true), false, builtAtMillis).service
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should contain (`Cache-Control`(NonEmptyList.of(`max-age`(31536000.seconds))))
    }
    it should "not cache them on dev mode" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/css/test.css"))).value.map(_.map(_.headers)).unsafeRunSync.getOrElse(Headers.empty) should not contain `Cache-Control`(NonEmptyList.of(`max-age`(31536000.seconds)))
    }
    it should "support fingerprinting" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = Uri.fromString(s"/css/test.$builtAtMillis.css").toOption.fold(uri("/"))(x => x))).value.map(_.map(_.status)).unsafeRunSync() should contain(Status.Ok)
    }
}
