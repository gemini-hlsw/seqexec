// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.EphemerisKeyType
import gem.parser.EphemerisKeyParsers
import gsp.math.syntax.parser._
import gsp.math.syntax.string._
import gsp.math.optics.Format

import cats.{ Order, Show }
import cats.implicits._
import monocle.macros.Lenses

/** Ephemeris data lookup key which uniquely identifies a non-sidreal object in
  * the database.
  */
sealed trait EphemerisKey extends Product with Serializable {

  import EphemerisKey.{ AsteroidNew, AsteroidOld, Comet, MajorBody, UserSupplied }

  /** Human readable desgination that discriminates among ephemeris keys of the
    * same type.
    */
  def des: String

  /** Extracts the ephemeris lookup key type.
    */
  def keyType: EphemerisKeyType =
    this match {
      case AsteroidNew(_)  => EphemerisKeyType.AsteroidNew
      case AsteroidOld(_)  => EphemerisKeyType.AsteroidOld
      case Comet(_)        => EphemerisKeyType.Comet
      case MajorBody(_)    => EphemerisKeyType.MajorBody
      case UserSupplied(_) => EphemerisKeyType.UserSupplied
    }

}


object EphemerisKey extends EphemerisOptics {

  /** Unique Horizons designation, which should allow for reproducible ephemeris
    * queries <b>if</b> the values passed to the constructors are extracted
    * correctly from search results.
    */
  sealed abstract class Horizons(val queryString: String) extends EphemerisKey

  /** Horizons designation for asteroids, old and new style.
    */
  sealed abstract class Asteroid(s: String) extends Horizons(s)

  /** Designation for a comet, in the current apparition. Example: `C/1973 E1`
    * for Kohoutek, yielding the query string `NAME=C/1973 E1;CAP`.
    */
  @Lenses final case class Comet(des: String) extends Horizons(s"NAME=$des;CAP")
  object Comet

  /** Designation for an asteroid under modern naming conventions. Example:
    * `1971 UC1` for 1896 Beer, yielding a query string `ASTNAM=1971 UC1`.
    */
  @Lenses final case class AsteroidNew(des: String) extends Asteroid(s"ASTNAM=$des")
  object AsteroidNew

  /** Designation for an asteroid under "old" naming conventions. These are
    * small numbers. Example: `4` for Vesta, yielding a query string `4;`
    */
  @Lenses final case class AsteroidOld(num: Int) extends Asteroid(s"$num;") {
    override def des: String =
      num.toString
  }
  object AsteroidOld

  /** Designation for a major body (planet or satellite thereof). These have
    * small numbers. Example: `606` for Titan, yielding a query string `606`.
    */
  @Lenses final case class MajorBody(num: Int) extends Horizons(s"$num") {
    override def des: String =
      num.toString
  }
  object MajorBody

  /** Identifies a user-supplied collection of ephemeris data, where the number
    * comes from a database sequence.
    */
  @Lenses final case class UserSupplied(id: Int) extends EphemerisKey {
    override def des: String =
      id.toString
  }
  object UserSupplied

  implicit val ShowEphemerisKey: Show[EphemerisKey] =
    Show.fromToString

  implicit val OrderEphemerisKey: Order[EphemerisKey] =
    Order.by(fromString.reverseGet)
}

trait EphemerisOptics { this: EphemerisKey.type =>

  val fromString: Format[String, EphemerisKey] =
    Format(
      EphemerisKeyParsers.ephemerisKey.parseExact,
      k => {
        val (keyType, des) = fromTypeAndDes.reverseGet(k)
        s"${keyType.tag}_${des}"
      }
    )

  val fromTypeAndDes: Format[(EphemerisKeyType, String), EphemerisKey] =
    Format(
      { case (t, des) =>
        t match {
          case EphemerisKeyType.Comet        => Some(Comet(des))
          case EphemerisKeyType.AsteroidNew  => Some(AsteroidNew(des))
          case EphemerisKeyType.AsteroidOld  => des.parseIntOption.map(AsteroidOld(_))
          case EphemerisKeyType.MajorBody    => des.parseIntOption.map(MajorBody(_))
          case EphemerisKeyType.UserSupplied => des.parseIntOption.map(UserSupplied(_))
        }
      },
      k => (k.keyType, k.des)
    )

}