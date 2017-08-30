// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.EphemerisKeyType
import gem.parser.EphemerisKeyParsers
import gem.syntax.parser._

import cats.{ Eq, Show }
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

  /** Exports an ephemeris key to a `String` in a format that can be read by the
    * `gem.parsers.EphemerisKeyParsers` method.
    */
  def format: String =
    s"${keyType.tag}_$des"
}


object EphemerisKey {

  /** Unique Horizons designation, which should allow for reproducible ephemeris
    * queries <b>if</b> the values passed to the constructors are extracted
    * correctly from search results.
    */
  sealed abstract class Horizons(val queryString: String) extends EphemerisKey

  /** Designation for a comet, in the current apparition. Example: `C/1973 E1`
    * for Kohoutek, yielding the query string `NAME=C/1973 E1;CAP`.
    */
  @Lenses final case class Comet(des: String) extends Horizons(s"NAME=$des;CAP")
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object Comet

  /** Designation for an asteroid under modern naming conventions. Example:
    * `1971 UC1` for 1896 Beer, yielding a query string `ASTNAM=1971 UC1`.
    */
  @Lenses final case class AsteroidNew(des: String) extends Horizons(s"ASTNAM=$des")
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object AsteroidNew

  /** Designation for an asteroid under "old" naming conventions. These are
    * small numbers. Example: `4` for Vesta, yielding a query string `4;`
    */
  @Lenses final case class AsteroidOld(num: Int) extends Horizons(s"$num;") {
    override def des: String =
      num.toString
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object AsteroidOld

  /** Designation for a major body (planet or satellite thereof). These have
    * small numbers. Example: `606` for Titan, yielding a query string `606`.
    */
  @Lenses final case class MajorBody(num: Int) extends Horizons(s"$num") {
    override def des: String =
      num.toString
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object MajorBody

  /** Identifies a user-supplied collection of ephemeris data, where the number
    * comes from a database sequence.
    */
  @Lenses final case class UserSupplied(id: Int) extends EphemerisKey {
    override def des: String =
      id.toString
  }
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object UserSupplied

  /** Parse an `EphemerisKey`.
    * @group Constructors
    */
  def parse(s: String): Option[EphemerisKey] =
    EphemerisKeyParsers.ephemerisKey.parseExact(s)

  /** Parse an `EphemerisKey`, raising an exception on failure.
    * @group Constructors
    */
  def unsafeFromString(s: String): EphemerisKey =
    parse(s).getOrElse(sys.error(s"invalid ephemeris key: $s"))

  /** Parse an `EphemerisKey` from a type and a human readable
    * designation.
    *
    * @param t ephemeris type
    * @param des human readable designation
    * @group Constructors
    */
  def fromTypeAndDes(t: EphemerisKeyType, des: String): Option[EphemerisKey] = {
    import mouse.all._

    t match {
      case EphemerisKeyType.Comet        => Some(Comet(des))
      case EphemerisKeyType.AsteroidNew  => Some(AsteroidNew(des))
      case EphemerisKeyType.AsteroidOld  => des.parseIntOption.map(AsteroidOld(_))
      case EphemerisKeyType.MajorBody    => des.parseIntOption.map(MajorBody(_))
      case EphemerisKeyType.UserSupplied => des.parseIntOption.map(UserSupplied(_))
    }
  }

  def unsafeFromTypeAndDes(t: EphemerisKeyType, des: String): EphemerisKey =
    fromTypeAndDes(t, des).getOrElse(sys.error(s"inavlid ephemeris key ${t}_$des"))

  implicit val ShowEphemerisKey: Show[EphemerisKey] =
    Show.fromToString

  implicit val EqualEphemerisKey: Eq[EphemerisKey] =
    Eq.fromUniversalEquals
}