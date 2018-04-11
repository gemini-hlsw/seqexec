// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.web.server.common

import cats.data.{NonEmptyList, OptionT}
import cats.effect.{IO, Sync}
import org.http4s.CacheDirective._
import org.http4s.MediaType._
import org.http4s.dsl.io._
import org.http4s.headers.{`Cache-Control`, `Content-Type`}
import org.http4s.server.middleware.GZip
import org.http4s.{Charset, HttpService, Request, Response, StaticFile}

import scala.concurrent.duration._

class StaticRoutes(index: String, devMode: Boolean, builtAtMillis: Long) {
  val oneYear: Int = 365 * 24 * 60 * 60

  private val cacheHeaders = if (devMode) List(`Cache-Control`(NonEmptyList.of(`no-cache`()))) else List(`Cache-Control`(NonEmptyList.of(`max-age`(oneYear.seconds))))

  private val indexResponse: IO[Response[IO]] =
    Ok(index).map(_.withContentType(`Content-Type`(`text/html`, Charset.`UTF-8`)).putHeaders(`Cache-Control`(NonEmptyList.of(`no-cache`()))))

  // Get a resource from a local file, useful for development
  def localResource[F[_]: Sync](path: String, req: Request[F]): OptionT[F, Response[F]] =
    StaticFile.fromResource(path, Some(req)).map(_.putHeaders())

  // Get a resource from a local file, used in production
  def embeddedResource[F[_]: Sync](path: String, req: Request[F]): OptionT[F, Response[F]] = {
    OptionT.fromOption(Option(getClass.getResource(path))).flatMap(StaticFile.fromURL(_, Some(req)))
  }

  implicit class ReqOps[F[_]](req: Request[F]) {
    private val timestampRegex = s"(.*)\\.$builtAtMillis\\.(.*)".r

    /**
      * If a request contains the timestamp remove it to find the original file name
      */
    def removeTimestamp(path: String): String =
      path match {
        case timestampRegex(b, e) => s"$b.$e"
        case xs                   => xs
      }

    def endsWith(exts: String*): Boolean = exts.exists(req.pathInfo.endsWith)

    def serve(path: String)(implicit F: Sync[F]): F[Response[F]] = {
      // To find scala.js generated files we need to go into the dir below, hopefully this can be improved
      localResource(removeTimestamp(path), req).orElse(embeddedResource(removeTimestamp(path), req))
        .map(_.putHeaders(cacheHeaders: _*))
        .getOrElse(Response.notFound[F])
    }
  }

  private val supportedExtension = List(".html", ".js", ".map", ".css", ".png", ".eot", ".svg", ".woff", ".woff2", ".ttf", ".mp3", ".ico")

  def service: HttpService[IO] = GZip { HttpService {
    case req if req.pathInfo == "/"                  => indexResponse
    case req if req.endsWith(supportedExtension: _*) => req.serve(req.pathInfo)
    // This maybe not desired in all cases but it helps to keep client side routing cleaner
    case req if !req.pathInfo.contains(".")          => indexResponse
  }}
}
