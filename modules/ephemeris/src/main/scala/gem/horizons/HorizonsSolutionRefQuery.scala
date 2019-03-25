// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
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

  // Comets and Asteroids have solution references.
  private val SolnRefKey = "soln ref.="

  private val solnRefParser: Parser[String] =
    (manyUntil(anyChar, string(SolnRefKey)) ~> stringOf1(noneOf(",\n")))

  // MajorBody objects don't have a solution reference proper, so we use the
  // revision date.
  private val RevisedKey = "Revised: "

  private val revisionParser: Parser[String] =
    (manyUntil(anyChar, string(RevisedKey)) ~> manyUntil(anyChar, string("  ")).map(_.mkString))

  /** Parses the given string for a solution reference. For comets and asteroids,
    * this will be an actual solution reference.  For major bodies it is a
    * revision date.  Either way we treat this as opaque data only of interest
    * for determining whether there is new data.
    *
    * @param k key used to determine the type of non-sidereal object
    * @param s header for a non-sidereal object of this type
    *
    * @return a solution reference, if found in the header
    */
  def parseSolutionRef(k: EphemerisKey.Horizons, s: String): Option[HorizonsSolutionRef] = {
    val parser = k match {
      case EphemerisKey.MajorBody(_) => revisionParser
      case _                         => solnRefParser
    }

    (parser.map(s => HorizonsSolutionRef(s.trim)) parseOnly s).option
  }

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
        HorizonsClient
          .fetch(reqParams)
          .map(parseSolutionRef(key, _))
    }

}
