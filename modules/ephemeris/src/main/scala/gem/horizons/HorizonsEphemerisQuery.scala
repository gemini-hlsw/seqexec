// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.{EphemerisKey, Semester}
import gem.enum.Site
import gem.math.Ephemeris
import gem.syntax.time._

import cats._
import cats.effect.IO
import cats.implicits._

import fs2.Stream

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


  // Utility to calculate the integer ceiling of n/d.
  private def intCeil(n: Long, d: Long): Long =
    if (n >= 0) (n + d - 1L) / d else n / d

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
        step:  Duration): List[HorizonsEphemerisQuery] = {

    // Horizons doesn't support a step length below 500 ms and FRACSEC gives
    // only ms precision.
    val stepʹ   = roundUpMs(step) max MinStepLen
    val stepMsʹ = stepʹ.toMillis

    // Total time covered by a single page with max elements.
    val maxPage  = stepʹ * MaxElements.toLong

    // Calculate (end time inclusive, element count) for a page starting at pStart.
    def pageEnd(pStart: Instant): (Instant, Int) = {
      val rem = roundUpMs(Duration.between(pStart, end))
      val cnt = if (rem >= maxPage) MaxElements.toLong else intCeil(rem.toMillis, stepMsʹ) + 1L

      (pStart + stepʹ * (cnt - 1), cnt.toInt)
    }

    Stream.unfold(start) { pStart =>
      if (pStart >= end)
        None
      else {
        val (pEnd, pCnt) = pageEnd(pStart)

        // Horizons queries must return >= 2 elements.  If next page would have
        // just 1 elem, shorten *this* query so that the next one has 2.
        val (_,     nCnt ) = pageEnd(pStart + maxPage)
        val (pEndʹ, pCntʹ) = if (nCnt === 1) (pEnd - stepʹ, pCnt - 1) else (pEnd, pCnt)

        Some((HorizonsEphemerisQuery(key, site, pStart, pEndʹ, pCntʹ), pEndʹ + stepʹ))
      }
    }.toList
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

