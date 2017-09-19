// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for Non-sidereal target lookup type.
 * @group Enumerations (Generated)
 */
sealed abstract class EphemerisKeyType(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object EphemerisKeyType {

  /** @group Constructors */ case object Comet extends EphemerisKeyType("Comet", "Comet", "Horizons Comet")
  /** @group Constructors */ case object AsteroidNew extends EphemerisKeyType("AsteroidNew", "Asteroid New", "Horizons Asteroid (New Format)")
  /** @group Constructors */ case object AsteroidOld extends EphemerisKeyType("AsteroidOld", "Asteroid Old", "Horizons Asteroid (Old Format)")
  /** @group Constructors */ case object MajorBody extends EphemerisKeyType("MajorBody", "Major Body", "Horizons Major Body")
  /** @group Constructors */ case object UserSupplied extends EphemerisKeyType("UserSupplied", "User Supplied", "User Supplied")

  /** All members of EphemerisKeyType, in canonical order. */
  val all: List[EphemerisKeyType] =
    List(Comet, AsteroidNew, AsteroidOld, MajorBody, UserSupplied)

  /** Select the member of EphemerisKeyType with the given tag, if any. */
  def fromTag(s: String): Option[EphemerisKeyType] =
    all.find(_.tag === s)

  /** Select the member of EphemerisKeyType with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): EphemerisKeyType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val EphemerisKeyTypeEnumerated: Enumerated[EphemerisKeyType] =
    new Enumerated[EphemerisKeyType] {
      def all = EphemerisKeyType.all
      def tag(a: EphemerisKeyType) = a.tag
    }

}