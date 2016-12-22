package edu.gemini.web.common.http4s

import edu.gemini.web.server.common.StaticRoutes
import org.http4s.CacheDirective.{`max-age`, `no-cache`}
import org.http4s.MediaType._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s.util.NonEmptyList
import org.http4s._
import org.scalatest.{EitherValues, FlatSpec, Matchers}

import scala.concurrent.duration._

class StaticRoutesSpec extends FlatSpec with Matchers with EitherValues with UriFunctions {

  val builtAtMillis = 1000L

  def index(devMode: Boolean) = {
    val deps = if (devMode) "package-jsdeps.js" else s"package-jsdeps.min.${builtAtMillis}.js"
    val script = if (devMode) s"app.js" else s"app-opt.${builtAtMillis}.js"

    val xml =
      <html lang="en">
        <head>
          <meta charset="utf-8"/>
          <meta http-equiv="X-UA-Compatible" content="IE=edge"/>

          <title>Title</title>

          <link rel="stylesheet" href={s"css/semantic.${builtAtMillis}.css"}/>
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
      service.apply(Request(uri = uri("/"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).unsafePerformSync.body.runLast.unsafePerformSync.map(_.decodeUtf8).get.right.value
      body should include regex ".*jsdeps\\.min\\.(\\d*)\\.js.*"
      body should include regex ".*app-opt\\.(\\d*)\\.js.*"
    }
    it should "return a generated index(true) with development files on dev mode" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).unsafePerformSync.body.runLast.unsafePerformSync.map(_.decodeUtf8).get.right.value
      body should include regex ".*jsdeps\\.js.*"
      body should include regex ".*app*\\.js.*"
    }

  "StaticRoutes regular files" should
    "return a file if present on resources" in {
      val service = new StaticRoutes(index(false), false, builtAtMillis).service
      service.apply(Request(uri = uri("/css/test.css"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/css/test.css"))).unsafePerformSync.headers should contain (`Content-Type`(`text/css`))
    }
    it should "return a 404 for an unknown file" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/unknown"))).unsafePerformSync.status should equal (Status.NotFound)
    }
    it should "not leak the application configuration file" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/app.conf"))).unsafePerformSync.status should equal (Status.NotFound)
    }
    it should "cache them for a year on production mode" in {
      val service = new StaticRoutes(index(true), false, builtAtMillis).service
      service.apply(Request(uri = uri("/css/test.css"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`max-age`(31536000.seconds))))
    }
    it should "not cache them on dev mode" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = uri("/css/test.css"))).unsafePerformSync.headers should not contain `Cache-Control`(NonEmptyList(`max-age`(31536000.seconds)))
    }
    it should "support fingerprinting" in {
      val service = new StaticRoutes(index(true), true, builtAtMillis).service
      service.apply(Request(uri = Uri.fromString(s"/css/test.$builtAtMillis.css").toOption.get)).unsafePerformSync.status should equal (Status.Ok)
    }
}
