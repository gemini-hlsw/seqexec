// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import atto.Atto._
import atto._
import cats.effect.IO
import gem.{EphemerisKey, HorizonsSolutionRef}

/** Horizons solution reference query.  Horizons supports a version number, of
  * sorts, for comet and asteriod ephemeris calculations.  The version is an
  * opaque `String` that can be used to compare for equality against previous
  * versions to determine if an update is required.  Unfortunately, major bodies
  * do not include solution references.
  *
  * {{{
  *   import gem.EphemerisKey
  *   import gem.horizons.HorizonsSolutionRefQuery
  *
  *   HorizonsSolutionRefQuery(EphemerisKey.Comet("81P")).lookup.unsafeRunSync
  * }}}
  *
  */
sealed trait HorizonsSolutionRefQuery {

  /** URL string corresponding to the horizons request. */
  def urlString: String

  /** Returns a program that will perform the solution reference lookup when
    * executed.
    */
  def lookup: IO[Option[HorizonsSolutionRef]]

}

object HorizonsSolutionRefQuery {

  private val FixedParams = HorizonsClient.SharedParams ++ Map(
    "MAKE_EPHEM" -> "NO"  // We don't need actual ephemeris elements
  )

  private val SolnRefKey = "soln ref.="

  private val solnRefParser: Parser[HorizonsSolutionRef] =
    (manyUntil(anyChar, string(SolnRefKey)) ~> stringOf1(noneOf(",\n"))).map { s =>
      HorizonsSolutionRef(s.trim)
    }

  /** Parses an horizons header string into a solution reference, if found.
    */
  def parseSolutionRef(s: String): Option[HorizonsSolutionRef] =
    (solnRefParser parseOnly s).option

  /** Creates a query instance for the given ephemeris key.
    */
  def apply(key: EphemerisKey.Horizons): HorizonsSolutionRefQuery =

    new HorizonsSolutionRefQuery {
      val reqParams = Map(
        "COMMAND" -> s"'${key.queryString}'"
      ) ++ FixedParams

      override val urlString: String =
        HorizonsClient.urlString(reqParams)

      override val lookup: IO[Option[HorizonsSolutionRef]] =
        key match {

          // Horizons has no solution references for major bodies so we can skip
          // doing an actual lookup.
          case EphemerisKey.MajorBody(_) =>
            IO.pure(None)

          case _                         =>
            HorizonsClient.fetch(reqParams).map(parseSolutionRef)
        }
    }

}
