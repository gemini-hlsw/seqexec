// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for horizons non-sidereal target type.
 * @group Enumerations (Generated)
 */
sealed abstract class HorizonsType(
  val tag: String,
  val shortName: String,
  val longName: String
)

object HorizonsType {

  /** @group Constructors */ case object Comet extends HorizonsType("Comet", "Comet", "Comet")
  /** @group Constructors */ case object AsteroidNew extends HorizonsType("AsteroidNew", "Asteroid New", "Asteroid (New Format)")
  /** @group Constructors */ case object AsteroidOld extends HorizonsType("AsteroidOld", "Asteroid Old", "Asteroid (Old Format)")
  /** @group Constructors */ case object MajorBody extends HorizonsType("MajorBody", "Major Body", "Major Body")

  /** All members of HorizonsType, in canonical order. */
  val all: List[HorizonsType] =
    List(Comet, AsteroidNew, AsteroidOld, MajorBody)

  /** Select the member of HorizonsType with the given tag, if any. */
  def fromTag(s: String): Option[HorizonsType] =
    all.find(_.tag === s)

  /** Select the member of HorizonsType with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): HorizonsType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val HorizonsTypeEnumerated: Enumerated[HorizonsType] =
    new Enumerated[HorizonsType] {
      def all = HorizonsType.all
      def tag(a: HorizonsType) = a.tag
    }

}