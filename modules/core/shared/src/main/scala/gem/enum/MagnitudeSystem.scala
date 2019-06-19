// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for magnitude system.
 * @group Enumerations (Generated)
 */
sealed abstract class MagnitudeSystem(
  val tag: String
) extends Product with Serializable

object MagnitudeSystem {

  /** @group Constructors */ case object Vega extends MagnitudeSystem("Vega")
  /** @group Constructors */ case object AB extends MagnitudeSystem("AB")
  /** @group Constructors */ case object Jy extends MagnitudeSystem("Jy")

  /** All members of MagnitudeSystem, in canonical order. */
  val all: List[MagnitudeSystem] =
    List(Vega, AB, Jy)

  /** Select the member of MagnitudeSystem with the given tag, if any. */
  def fromTag(s: String): Option[MagnitudeSystem] =
    all.find(_.tag === s)

  /** Select the member of MagnitudeSystem with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): MagnitudeSystem =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"MagnitudeSystem: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val MagnitudeSystemEnumerated: Enumerated[MagnitudeSystem] =
    new Enumerated[MagnitudeSystem] {
      def all = MagnitudeSystem.all
      def tag(a: MagnitudeSystem) = a.tag
      override def unsafeFromTag(s: String): MagnitudeSystem =
        MagnitudeSystem.unsafeFromTag(s)
    }

}