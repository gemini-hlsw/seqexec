// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.parser

import cats.implicits._
import atto._, Atto._
import gem.math._

/** Parsers for [[gem.math.Coordinates]] and related types. */
trait CoordinateParsers {
  import AngleParsers.{ hms, dms }
  import MiscParsers.spaces1

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val ra: Parser[RightAscension] =
    hms.map(RightAscension(_)) named "ra"

  /** Parser for a RightAscension, always a positive angle in HMS. */
  val dec: Parser[Declination] =
    dms.map(Declination.fromAngle.getOption).flatMap {
      case Some(ra) => ok(ra)
      case None     => err[Declination]("Invalid Declination")
    } named "dec"

  /** Parser for coordinates: HMS and DMS separated by spaces. */
  val coordinates: Parser[Coordinates] =
    (ra <~ spaces1, dec).mapN(Coordinates(_, _)) named "coordinates"

}
object CoordinateParsers extends CoordinateParsers
