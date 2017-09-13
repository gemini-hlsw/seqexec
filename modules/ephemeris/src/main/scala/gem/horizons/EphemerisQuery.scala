// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.{ EphemerisKey, Semester }
import gem.enum.Site
import gem.math.Ephemeris

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


/** Representation of an Horizons ephemeris query.
  */
sealed trait EphemerisQuery {

  /** URL string corresponding the the horizons request. */
  def urlString: String

  /** Makes an horizons request and parses all the ephemeris elements in memory.
    */
  def fetchEphemeris: IO[Either[String, Ephemeris]]

  /** Makes an horizons request, parsing and emitting ephemeris elements as they
    * are received.
    */
  def streamEphemeris: Stream[IO, Ephemeris.Element]

}

object EphemerisQuery {
  type HorizonsDesignation = EphemerisKey.Horizons

  /** Maximum number of elements that may be received by an Horizons ephemeris
    * request.
    */
  val MaxElements: Int = 90024

  // Horizons permits only one request at a time from a given host.
  private val client = PooledHttp1Client[IO](maxTotalConnections = 1)

  private val Url         = "https://ssd.jpl.nasa.gov/horizons_batch.cgi"
  private val DateFormat  = DateTimeFormatter.ofPattern("yyyy-MMM-d HH:mm:ss.SSS", US).withZone(UTC)

  private val FixedParams = List(
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

  // Format coordinates and altitude as expected by horizons.
  private def formatCoords(s: Site): String =
    f"'${s.longitude.toDoubleDegrees}%1.5f,${s.latitude.toSignedDoubleDegrees}%1.6f,${s.altitude.toDouble/1000.0}%1.3f'"

  // URL encode query params.
  private def formatQuery(params: List[(String, String)]): String =
    params
      .map { case (k, v) => s"$k=${URLEncoder.encode(v, UTF_8.name)}" }
      .mkString("&")

  /** Constructs an horizons ephemeris query for the given key, site, start/stop
    * time range, and element limit.
    *
    * @param key   Unique Horizons designation for the non-sidereal target of
    *              interest
    * @param site  site to which the ephemeris data applies
    * @param start time at which ephemeris data should start
    * @param end   time at which ephemeris data should end
    * @param limit count of elements requested (note that a successful query
    *              will contain no more than 90024 elements regardless of the
    *              requested limit)
    *
    * @return an Horizons query ready to be executed
    */
  def apply(key:   HorizonsDesignation,
            site:  Site,
            start: Instant,
            end:   Instant,
            limit: Int): EphemerisQuery =

    new EphemerisQuery {
      // Horizons responses are capped at 90024, but always include one more than
      // requested so the max request is MaxElements - 1.
      val reqParams = List(
        "COMMAND"    -> s"'${key.queryString}'",
        "SITE_COORD" -> formatCoords(site),
        "START_TIME" -> s"'${DateFormat.format(start)}'",
        "STEP_SIZE"  -> (((limit max 2) min MaxElements) - 1).toString,
        "STOP_TIME"  -> s"'${DateFormat.format(end)}'"
      ) ++ FixedParams

      override val urlString: String =
        s"$Url?${formatQuery(reqParams)}"

      /** Creates an http4s Uri representing the request and lifts it into IO,
        * raising an error if there is a problem parsing the request.
        */
      val uri: IO[Uri] =
        Uri.fromString(urlString).fold(IO.raiseError, IO.pure)

      /** Creates an http4s Request representing the request. */
      val request: IO[Request[IO]] =
          uri.map { Request(Method.GET, _) }

      override val fetchEphemeris: IO[Either[String, Ephemeris]] =
        client.expect[String](request).map { s =>
          EphemerisParser.parse(s).either
        }

      override val streamEphemeris: Stream[IO, Ephemeris.Element] =
        client.streaming(request) {
          _.body.through(utf8Decode).through(EphemerisParser.elements)
        }
    }

  /** Constructs an horizons ephemeris query for the given key, site, semester,
    * and element limit.
    *
    * @param key      Unique Horizons designation for the non-sidereal target of
    *                 interest
    * @param site     site to which the ephemeris data applies
    * @param semester six month period to include
    * @param limit    count of elements requested (note that a successful query
    *                 will contain no more than 90024 elements regardless of the
    *                 requested limit)
    *
    * @return an Horizons query ready to be executed
    */
  def forSemester(key:      HorizonsDesignation,
                  site:     Site,
                  semester: Semester,
                  limit:    Int): EphemerisQuery =
    apply(
      key,
      site,
      semester.start.localDateTime.toInstant(UTC),
      semester.end.localDateTime.toInstant(UTC),
      limit
    )

  /** Constructs an horizons ephemeris query for the given key, site, current
    * semester, and element limit.
    *
    * @param key   Unique Horizons designation for the non-sidereal target of
    *              interest
    * @param site  site to which the ephemeris data applies
    * @param limit count of elements requested (note that a successful query
    *              will contain no more than 90024 elements regardless of the
    *              requested limit)
    *
    * @return an IO that computes an Horizons query ready to be executed
    */
  def forCurrentSemester(key:   HorizonsDesignation,
                         site:  Site,
                         limit: Int): IO[EphemerisQuery] =

    Semester.current.map(forSemester(key, site, _, limit))


  /** Constructs an horizons ephemeris query for the given key, site, current
    * semester, and element limit.  The results include 1 month padding on
    * either side of the current semester.
    *
    * @param key   Unique Horizons designation for the non-sidereal target of
    *              interest
    * @param site  site to which the ephemeris data applies
    * @param limit count of elements requested (note that a successful query
    *              will contain no more than 90024 elements regardless of the
    *              requested limit)
    *
    * @return an IO that computes an Horizons query ready to be executed
    */
  def forCurrentSemesterWithPadding(key:   HorizonsDesignation,
                                    site:  Site,
                                    limit: Int): IO[EphemerisQuery] =

    Semester.current.map { s =>
      apply(
        key,
        site,
        s.start.localDateTime.minusMonths(1L).toInstant(UTC),
        s.end.localDateTime.plusMonths(1L).toInstant(UTC),
        limit
      )
    }

}

