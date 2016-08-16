package edu.gemini.seqexec.web.http4s

import edu.gemini.seqexec.web.server.http4s.StaticRoutes
import org.http4s.CacheDirective.`no-cache`
import org.http4s.MediaType._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s.util.NonEmptyList
import org.http4s.{Charset, Request, Status, UriFunctions}
import org.scalatest.{EitherValues, FlatSpec, Matchers}

class StaticRoutesSpec extends FlatSpec with Matchers with EitherValues with UriFunctions {
  "StaticRoutes index" should
    "Return a generated index with development files on dev mode" in {
      val service = new StaticRoutes(false).service
      service.apply(Request(uri = uri("/"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).unsafePerformSync.body.runLast.unsafePerformSync.map(_.decodeUtf8).get.right.value
      body should include regex ".*jsdeps\\.min\\.js.*"
      body should include regex ".*seqexec-opt\\.js.*"
    }
    it should "Return a generated index with production files on production mode" in {
      val service = new StaticRoutes(true).service
      service.apply(Request(uri = uri("/"))).unsafePerformSync.status should equal (Status.Ok)
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Content-Type`(`text/html`, Charset.`UTF-8`))
      service.apply(Request(uri = uri("/"))).unsafePerformSync.headers should contain (`Cache-Control`(NonEmptyList(`no-cache`())))
      val body = service.apply(Request(uri = uri("/"))).unsafePerformSync.body.runLast.unsafePerformSync.map(_.decodeUtf8).get.right.value
      body should include regex ".*jsdeps\\.js.*"
      body should include regex ".*seqexec\\.js.*"
    }

}
