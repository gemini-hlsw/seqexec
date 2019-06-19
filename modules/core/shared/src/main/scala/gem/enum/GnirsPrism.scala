// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GNIRS Prism Turret.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsPrism(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable {
  type Self = this.type
}

object GnirsPrism {

  type Aux[A] = GnirsPrism { type Self = A }

  /** @group Constructors */ case object Mirror extends GnirsPrism("Mirror", "Mirror", "Mirror")
  /** @group Constructors */ case object Sxd extends GnirsPrism("Sxd", "Short XD", "Short cross dispersion")
  /** @group Constructors */ case object Lxd extends GnirsPrism("Lxd", "Long XD", "Long cross dispersion")

  /** All members of GnirsPrism, in canonical order. */
  val all: List[GnirsPrism] =
    List(Mirror, Sxd, Lxd)

  /** Select the member of GnirsPrism with the given tag, if any. */
  def fromTag(s: String): Option[GnirsPrism] =
    all.find(_.tag === s)

  /** Select the member of GnirsPrism with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsPrism =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GnirsPrismEnumerated: Enumerated[GnirsPrism] =
    new Enumerated[GnirsPrism] {
      def all = GnirsPrism.all
      def tag(a: GnirsPrism) = a.tag
    }

}