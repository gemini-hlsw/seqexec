package edu.gemini.seqexec.web.http4s

import edu.gemini.seqexec.web.server.OcsBuildInfo
import edu.gemini.seqexec.web.server.http4s.StaticRoutes
import org.http4s.CacheDirective.{`max-age`, `no-cache`}
import org.http4s.MediaType._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s.util.NonEmptyList
import org.http4s._
import org.scalatest.{EitherValues, FlatSpec, Matchers}

import scala.concurrent.duration._

class StaticRoutesSpec extends FlatSpec with Matchers with EitherValues with UriFunctions {
  "StaticRoutes index" should
    "return a generated index with production files on production mode" in {
      val service = new StaticRoutes(false).service
      service.apply(Request(uri = uri("/"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).unsafePerformSync.body.runLast.unsafePerformSync.map(_.decodeUtf8).get.right.value
      body should include regex ".*jsdeps\\.min\\.(\\d*)\\.js.*"
      body should include regex ".*seqexec-opt\\.(\\d*)\\.js.*"
    }
    it should "return a generated index with development files on dev mode" in {
      val service = new StaticRoutes(true).service
      service.apply(Request(uri = uri("/"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).unsafePerformSync.body.runLast.unsafePerformSync.map(_.decodeUtf8).get.right.value
      body should include regex ".*jsdeps\\.js.*"
      body should include regex ".*seqexec\\.js.*"
    }

  "StaticRoutes regular files" should
    "return a file if present on resources" in {
      val service = new StaticRoutes(false).service
      service.apply(Request(uri = uri("/css/semantic.css"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/css/semantic.css"))).unsafePerformSync.headers should contain (`Content-Type`(`text/css`))
    }
    it should "return a 404 for an unknown file" in {
      val service = new StaticRoutes(true).service
      service.apply(Request(uri = uri("/unknown"))).unsafePerformSync.status should equal (Status.NotFound)
    }
    it should "not leak the application configuration file" in {
      val service = new StaticRoutes(true).service
      service.apply(Request(uri = uri("/app.conf"))).unsafePerformSync.status should equal (Status.NotFound)
    }
    it should "cache them for a year on production mode" in {
      val service = new StaticRoutes(false).service
      service.apply(Request(uri = uri("/css/semantic.css"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`max-age`(31536000.seconds))))
    }
    it should "not cache them on dev mode" in {
      val service = new StaticRoutes(true).service
      service.apply(Request(uri = uri("/css/semantic.css"))).unsafePerformSync.headers should not contain `Cache-Control`(NonEmptyList(`max-age`(31536000.seconds)))
    }
    it should "support fingerprinting" in {
      val service = new StaticRoutes(true).service
      service.apply(Request(uri = Uri.fromString(s"/css/semantic.${OcsBuildInfo.builtAtMillis}.css").toOption.get)).unsafePerformSync.status should equal (Status.Ok)
    }
}
