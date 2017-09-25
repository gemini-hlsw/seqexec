// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.{EphemerisKey, Semester}
import gem.enum.Site
import gem.math.Ephemeris
import cats.implicits._
import cats.effect.IO
import fs2.Stream

import java.time.Instant
import java.time.ZoneOffset.UTC


/** Representation of an Horizons ephemeris query.
  */
sealed trait EphemerisQuery {

  /** URL string corresponding the the horizons request. */
  def urlString: String

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

  private val FixedParams = HorizonsClient.SharedParams ++ Map(
    "CENTER"      -> "coord",
    "COORD_TYPE"  -> "GEODETIC",
    "extra_prec"  -> "YES",
    "MAKE_EPHEM"  -> "YES",
    "QUANTITIES"  -> "1",
    "time_digits" -> "FRACSEC"
  )

  /** Constructs an horizons ephemeris query for the given key, site, start/stop
    * time range, and element limit. Note that a successful query will contain
    * no more than 90024 elements regardless of the requested limit since
    * Horizons responses are capped at 90024 elements.  It will also include at
    * least two elements (at the start and stop times).
    *
    * @param key   Unique Horizons designation for the non-sidereal target of
    *              interest
    * @param site  site to which the ephemeris data applies
    * @param start time at which ephemeris data should start (inclusive)
    * @param end   time at which ephemeris data should end (inclusive)
    * @param limit count of elements requested
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
      val reqParams = Map(
        "COMMAND"    -> s"'${key.queryString}'",
        "SITE_COORD" -> HorizonsClient.formatCoords(site),
        "START_TIME" -> s"'${HorizonsClient.formatInstant(start)}'",
        "STEP_SIZE"  -> (((limit max 2) min MaxElements) - 1).toString,
        "STOP_TIME"  -> s"'${HorizonsClient.formatInstant(end)}'"
      ) ++ FixedParams

      override val urlString: String =
        HorizonsClient.urlString(reqParams)

      override val streamEphemeris: Stream[IO, Ephemeris.Element] =
        HorizonsClient.stream(reqParams).through(EphemerisParser.elements)
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

    Semester.current(site).map(forSemester(key, site, _, limit))


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

    Semester.current(site).map { s =>
      apply(
        key,
        site,
        s.start.localDateTime.minusMonths(1L).toInstant(UTC),
        s.end.localDateTime.plusMonths(1L).toInstant(UTC),
        limit
      )
    }

}

