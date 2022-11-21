// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.server.common

import scala.concurrent.duration._
import cats.data.NonEmptyList
import cats.data.OptionT
import cats.effect.Sync
import cats.instances.string._
import cats.syntax.eq._
import org.http4s.CacheDirective._
import org.http4s.HttpRoutes
import org.http4s.Request
import org.http4s.Response
import org.http4s.StaticFile
import org.http4s.headers.`Cache-Control`
import org.http4s.server.middleware.GZip

class StaticRoutes[F[_]: Sync](
  devMode:       Boolean,
  builtAtMillis: Long
) {
  val oneYear: Int = 365 * 24 * 60 * 60 // One year in seconds

  private val cacheHeaders =
    if (devMode) List(`Cache-Control`(NonEmptyList.of(`no-cache`())))
    else List(`Cache-Control`(NonEmptyList.of(`max-age`(oneYear.seconds))))

  // Get a resource from a local file, useful for development
  def localResource(path: String, req: Request[F]): OptionT[F, Response[F]] =
    StaticFile.fromResource(path, Some(req)).map(_.putHeaders())

  // Get a resource from a local file, used in production
  def embeddedResource(path: String, req: Request[F]): OptionT[F, Response[F]] =
    OptionT
      .fromOption(Option(getClass.getResource(path)))
      .flatMap(StaticFile.fromURL(_, Some(req)))

  implicit class ReqOps(req: Request[F]) {
    private val timestampRegex = s"(.*)\\.$builtAtMillis\\.(.*)".r

    /**
     * If a request contains the timestamp remove it to find the original file name
     */
    def removeTimestamp(path: String): String =
      path match {
        case timestampRegex(b, e) => s"$b.$e"
        case xs                   => xs
      }

    def endsWith(exts: String*): Boolean = exts.exists(req.pathInfo.toString.endsWith)

    def serve(path: String): F[Response[F]] =
      // To find scala.js generated files we need to go into the dir below, hopefully this can be improved
      localResource(removeTimestamp(path), req)
        .orElse(embeddedResource(removeTimestamp(path), req))
        .map(x => x.putHeaders(cacheHeaders))
        .getOrElse(Response.notFound[F])
  }

  private val supportedExtension = List(".html",
                                        ".js",
                                        ".map",
                                        ".css",
                                        ".png",
                                        ".eot",
                                        ".svg",
                                        ".woff",
                                        ".woff2",
                                        ".ttf",
                                        ".mp3",
                                        ".ico",
                                        "webm"
  )

  def service: HttpRoutes[F] = GZip {
    HttpRoutes.of[F] {
      case req if req.pathInfo.toString() === "/"      => req.serve("/index.html")
      case req if req.endsWith(supportedExtension: _*) => req.serve(req.pathInfo.toString)
      // This maybe not desired in all cases but it helps to keep client side routing cleaner
      case req if !req.pathInfo.toString.contains(".") => req.serve("/index.html")
    }
  }
}
