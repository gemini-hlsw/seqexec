// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.EphemerisKey
import gem.enum.Site, Site.GN, Site.GS
import gem.math.Ephemeris

import cats._
import cats.implicits._
import cats.effect.IO

import fs2.Stream
import fs2.text.utf8Decode

import org.http4s._
import org.http4s.client.blaze.PooledHttp1Client

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8
import java.time.Instant
import java.time.ZoneOffset.UTC
import java.time.format.DateTimeFormatter
import java.util.Locale.US

/**
  *
  */
object EphemerisQuery {

  type HorizonsDesignation = EphemerisKey.Horizons

  private val client = PooledHttp1Client[IO]()

  private object req {
    val url         = "https://ssd.jpl.nasa.gov/horizons_batch.cgi"
    val dateFormat  = DateTimeFormatter.ofPattern("yyyy-MMM-d HH:mm:ss.SSS", US).withZone(UTC)

    val fixedParams = List(
      "batch"       -> "1",
      "CENTER"      -> "coord",
      "COORD_TYPE"  -> "GEODETIC",
      "CSV_FORMAT"  -> "NO",
      "extra_prec"  -> "YES",
      "MAKE_EPHEM"  -> "YES",
      "QUANTITIES"  -> "1",
      "TABLE_TYPE"  -> "OBSERVER",
      "time_digits" -> "FRACSEC"
    )

    // TODO: take from Site enum
    def formatCoords(s: Site): String =
      s match {
        case GN => "'204.53094,19.823806,4.2134'"
        case GS => "'289.23944,-30.237778,2.743'"
      }

    def formatQuery(params: List[(String, String)]): String =
      params
        .map { case (k, v) => s"$k=${URLEncoder.encode(v, UTF_8.name)}" }
        .mkString("&")

    def string(k: EphemerisKey.Horizons, s: Site, start: Instant, end: Instant, elements: Int): String = {

      val reqParams = List(
        "COMMAND"    -> s"'${k.queryString}'",
        "SITE_COORD" -> formatCoords(s),
        "START_TIME" -> s"'${dateFormat.format(start)}'",
        "STEP_SIZE"  -> (((elements max 2) min 90024) - 1).toString,
        "STOP_TIME"  -> s"'${dateFormat.format(end)}'"
      )

      val str = s"$url?${formatQuery(reqParams ++ fixedParams)}"

      println(str)

      str
    }

    def uri[F[_]](k: HorizonsDesignation, s: Site, start: Instant, end: Instant, elements: Int)(implicit F: MonadError[F, Throwable]): F[Uri] =
      Uri.fromString(string(k, s, start, end, elements)).fold(F.raiseError, F.pure)

    def apply[F[_]](k: HorizonsDesignation, s: Site, start: Instant, end: Instant, elements: Int)(implicit ev: MonadError[F, Throwable]): F[Request[F]] =
      uri[F](k, s, start, end, elements).map { Request[F](Method.GET, _) }
  }

  /** Makes an horizons request and parses all the ephemeris elements in memory.
    */
  def fetchEphemeris(k: HorizonsDesignation, s: Site, start: Instant, end: Instant, elements: Int): IO[Either[String, Ephemeris]] =
    client.expect[String](req[IO](k, s, start, end, elements)).map { s =>
      EphemerisParser.parse(s).either
    }

  /** Makes an horizons request, parsing and emitting ephemeris elements as they
    * are received.
    */
  def streamEphemeris(k: HorizonsDesignation, s: Site, start: Instant, end: Instant, elements: Int): Stream[IO, Ephemeris.Element] =
    client.streaming(req[IO](k, s, start, end, elements)) {
      _.body.through(utf8Decode).through(EphemerisParser.elements)
    }
}
