// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.HorizonsType
import gem.parser.HorizonsDesignationParsers
import gem.syntax.parser._

import cats.{ Eq, Show }

import monocle.macros.Lenses

/**
 * Unique Horizons designation, which should allow for reproducible ephemeris queries <b>if</b> the
 * values passed to the constructors are extracted correctly from search results.
 */
sealed abstract class HorizonsDesignation(val queryString: String) extends Product with Serializable {

  def des: String // designation, human readable

  import HorizonsDesignation._

  /** Extracts the horizons non-sidereal type from the designation.
    */
  def horizonsType: HorizonsType =
    this match {
      case Comet(_)       => HorizonsType.Comet
      case AsteroidNew(_) => HorizonsType.AsteroidNew
      case AsteroidOld(_) => HorizonsType.AsteroidOld
      case MajorBody(_)   => HorizonsType.MajorBody
    }

  /** Exports an HorizonsDesignation to a String in a format that can be read
    * by the `gem.parsers.HorizonsDesignationParsers` method.
    */
  def format: String =
    s"${horizonsType.tag}_$des"
}

object HorizonsDesignation {

  /**
   * Designation for a comet, in the current apparition. Example: `C/1973 E1` for Kohoutek, yielding
   * the query string `NAME=C/1973 E1;CAP`.
   */
  @Lenses final case class Comet(des: String) extends HorizonsDesignation(s"NAME=$des;CAP")
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object Comet

  sealed abstract class Asteroid(s: String) extends HorizonsDesignation(s)

  /**
   * Designation for an asteroid under modern naming conventions. Example: `1971 UC1` for
   * 1896 Beer, yielding a query string `ASTNAM=1971 UC1`.
   */
  @Lenses final case class AsteroidNew(des: String) extends Asteroid(s"ASTNAM=$des")
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object AsteroidNew

  /**
   * Designation for an asteroid under "old" naming conventions. These are small numbers. Example:
   * `4` for Vesta, yielding a query string `4;`
   */
  @Lenses final case class AsteroidOld(num: Int) extends Asteroid(s"$num;") {
    override def des: String =
      num.toString
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object AsteroidOld

  /**
   * Designation for a major body (planet or satellite thereof). These have small numbers. Example:
   * `606` for Titan, yielding a query string `606`.
   */
  @Lenses final case class MajorBody(num: Int) extends HorizonsDesignation(s"$num") {
    override def des: String =
      num.toString
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object MajorBody

  /**
    * Parse an `HorizonsDesignation`.
    * @group Constructors
    */
  def parse(s: String): Option[HorizonsDesignation] =
    HorizonsDesignationParsers.horizonsDesignation.parseExact(s)

  /**
    * Parse an `HorizonsDesignation`, raising an exception on failure.
    * @group Constructors
    */
  def unsafeFromString(s: String): HorizonsDesignation =
    parse(s).getOrElse(sys.error(s"invalid horizons designation: $s"))

  /**
    * Parse an `HorizonsDesignation` from a type and a human readable
    * designation.
    *
    * @param t horizons type
    * @param des human readable designation
    * @group Constructors
    */
  def fromTypeAndDes(t: HorizonsType, des: String): Option[HorizonsDesignation] = {
    import mouse.all._

    t match {
      case HorizonsType.Comet       => Some(Comet(des))
      case HorizonsType.AsteroidNew => Some(AsteroidNew(des))
      case HorizonsType.AsteroidOld => des.parseIntOption.map(AsteroidOld(_))
      case HorizonsType.MajorBody   => des.parseIntOption.map(MajorBody(_))
    }
  }

  def unsafeFromTypeAndDes(t: HorizonsType, des: String): HorizonsDesignation =
    fromTypeAndDes(t, des).getOrElse(sys.error(s"inavlid horizons designation ${t}_$des"))

  implicit val ShowHorizonsDesignation: Show[HorizonsDesignation] =
    Show.fromToString

  implicit val EqualHorizonsDesignation: Eq[HorizonsDesignation] =
    Eq.fromUniversalEquals
}
