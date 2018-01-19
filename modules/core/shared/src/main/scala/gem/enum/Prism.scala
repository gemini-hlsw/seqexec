// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for Prism turret.
 * @group Enumerations (Generated)
 */
sealed abstract class Prism(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object Prism {

  /** @group Constructors */ case object Mirror extends Prism("Mirror", "Mirror", "Mirror")
  /** @group Constructors */ case object Sxd extends Prism("Sxd", "Short XD", "Short cross dispersion")
  /** @group Constructors */ case object Lxd extends Prism("Lxd", "Long XD", "Long cross dispersion")

  /** All members of Prism, in canonical order. */
  val all: List[Prism] =
    List(Mirror, Sxd, Lxd)

  /** Select the member of Prism with the given tag, if any. */
  def fromTag(s: String): Option[Prism] =
    all.find(_.tag === s)

  /** Select the member of Prism with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): Prism =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val PrismEnumerated: Enumerated[Prism] =
    new Enumerated[Prism] {
      def all = Prism.all
      def tag(a: Prism) = a.tag
    }

}