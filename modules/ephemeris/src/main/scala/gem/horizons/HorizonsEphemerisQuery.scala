// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.{EphemerisKey, Semester}
import gem.enum.Site
import gem.math.Ephemeris
import gsp.math.syntax.time._

import cats._
import cats.effect.IO
import cats.implicits._

import fs2.Stream
import mouse.boolean._

import java.time.{Duration, Instant, Period}

import scala.math.Ordering.Implicits._


/** Representation of an Horizons ephemeris query.
  */
sealed trait HorizonsEphemerisQuery {

  /** Query start time.  The first element returned should have this time.
    */
  def startTime: Instant

  /** Query end time.  The last element returned should have this time.
    */
  def endTime: Instant

  /** Requested element limit.
    */
  def elementLimit: Int

  /** URL string corresponding the the horizons request.
    */
  def urlString: String

  /** A stream of ephemeris elements from horizons.
    */
  def streamEphemeris: Stream[IO, Ephemeris.Element]

}

object HorizonsEphemerisQuery {

  type HorizonsDesignation = EphemerisKey.Horizons

  /** Maximum number of elements that may be received by an Horizons ephemeris
    * request.
    */
  val MaxElements: Int =
    90024

  /** The minimum time between elements supported by an horizons request.
    */
  val MinStepLen: Duration =
    Duration.ofMillis(500)

  private val FixedParams = HorizonsClient.SharedParams ++ Map(
    "CENTER"      -> "coord",
    "COORD_TYPE"  -> "GEODETIC",
    "extra_prec"  -> "YES",
    "MAKE_EPHEM"  -> "YES",
    "QUANTITIES"  -> "'1,3'",   // RA/Dec (1) and rate of change (3)
    "time_digits" -> "FRACSEC"  // millisecond precision
  )

  /** Constructs an horizons ephemeris query for the given key, site, start/stop
    * time range, and element limit. Note that a successful query will contain
    * no more than 90024 elements regardless of the requested limit since
    * Horizons responses are capped at 90024.  It will also include at least two
    * elements (at the start and stop times). Finally, all elements must be
    * separated by at least 500ms or else the query fails.
    *
    * @param key   Unique Horizons designation for the non-sidereal target of
    *              interest
    * @param site  site to which the ephemeris data applies
    * @param start time at which ephemeris data should start (inclusive)
    * @param end   time at which ephemeris data should end (inclusive)
    * @param limit maximum number of elements requested
    *
    * @return an Horizons query ready to be executed
    */
  def apply(key:   HorizonsDesignation,
            site:  Site,
            start: Instant,
            end:   Instant,
            limit: Int): HorizonsEphemerisQuery =

    new HorizonsEphemerisQuery {

      override val startTime: Instant =
        start

      override val endTime: Instant =
        end

      override val elementLimit: Int =
        limit

      val reqParams = Map(
        "COMMAND"    -> s"'${key.queryString}'",
        "SITE_COORD" -> HorizonsClient.formatCoords(site),
        "START_TIME" -> s"${HorizonsClient.formatInstant(start)}",
        "STEP_SIZE"  -> (((limit max 2) min MaxElements) - 1).toString, // horizons wants # intervals -- the gap count not the element count
        "STOP_TIME"  -> s"${HorizonsClient.formatInstant(end)}"
      ) ++ FixedParams

      override val urlString: String =
        HorizonsClient.urlString(reqParams)

      override val streamEphemeris: Stream[IO, Ephemeris.Element] =
        HorizonsClient.stream(reqParams).through(EphemerisParser.elements)
    }

  // Utility to round up a Duration up to the next ms.
  private def roundUpMs(d: Duration): Duration =
    (d.getNano % 1000000) match {
      case 0 => d
      case i => d.plusNanos(1000000L - i.toLong)
    }

  /** Constructs a List of queries for the given key, site, time range, and step
    * size such that the entire range is covered by the combination of the
    * returned queries. Horizons places a max element count limit on its results
    * so this method provides a way to obtain all the queries necessary to cover
    * a time range with a given step size.
    *
    * Note, Horizons requires a minimum step size of 500ms so 500ms will be used
    * if a smaller value for `step` is requested and sub-millisecond fractions
    * are rounded up to the next millisecond regardless.
    *
    * @param key   Unique Horizons designation for the non-sidereal target of
    *              interest
    * @param site  site to which the ephemeris data applies
    * @param start start time for the first ephemeris element to be returned
    * @param end   nominal end time (the actual last element may happen after
    *              end if the step size doesn't evenly divide the total time)
    * @param step  time (ms precision) between successive ephemeris elements (a
    *              minimum of 500ms will be used if a smaller time is requested)
    *
    * @return List of queries that cover the entire time range
    */
  def paging(
    key:   HorizonsDesignation,
    site:  Site,
    start: Instant,
    end:   Instant,
    step:  Duration
  ): List[HorizonsEphemerisQuery] = {

    // Calculates the list of queries with adjusted end time and step size.
    def calc(end: Instant, step: Duration): List[HorizonsEphemerisQuery] = {
      val maxPageDuration = step * (MaxElements - 1).toLong

      Stream.unfold(start) { s =>               // page start time
        (s <= end).option {
          val e = (s + maxPageDuration) min end // page end time
          val c = (Duration.between(s, e).toMillis / step.toMillis + 1).toInt // element count
          // If the last query would produce just one element, which horizons
          // doesn't seem to support, just ask for two elements instead.
          val (eʹ, cʹ) = if (c === 1) (e + step, 2) else (e, c)
          (HorizonsEphemerisQuery(key, site, s, eʹ, cʹ), eʹ + step)
        }
      }.toList
    }

    // Horizons doesn't support a step length below 500 ms and FRACSEC gives
    // only ms precision.
    val stepʹ = roundUpMs(step) max MinStepLen

    // Adjust end to be an even number of multiples of the step interval ending
    // exactly at or just beyond the given end instant.  That is, it must cover
    // the entire range and be an even number of steps even if we have to go a
    // bit over the given end time.
    val endʹ  = {
      val stepMs  = stepʹ.toMillis
      val totalMs = roundUpMs(Duration.between(start, end)).toMillis
      val rem     = totalMs % stepMs
      start + Duration.ofMillis(totalMs + (if (rem === 0) 0 else stepMs - rem))
    }

    if (end <= start) List.empty else calc(endʹ, stepʹ)
  }

  def pagingSemester(
        key:      HorizonsDesignation,
        site:     Site,
        semester: Semester,
        step:     Duration,
        padding:  Period): List[HorizonsEphemerisQuery] = {

    val start = semester.start.atSite(site).minus(padding).toInstant
    val end   = semester.end.atSite(site).plus(padding).toInstant

    paging(key, site, start, end, step)
  }

  def pagingCurrentSemester(
        key:      HorizonsDesignation,
        site:     Site,
        step:     Duration,
        padding:  Period): IO[List[HorizonsEphemerisQuery]] =

    Semester.current(site).map(pagingSemester(key, site, _, step, padding))

  def streamAll(qs: List[HorizonsEphemerisQuery]): Stream[IO, Ephemeris.Element] =
    Monoid.combineAll(qs.map(_.streamEphemeris))
}

