// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Filter.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val band: MagnitudeBand,
  val obsolete: Boolean
) extends Product with Serializable

object GpiFilter {

  /** @group Constructors */ case object Y extends GpiFilter("Y", "Y", "Y", MagnitudeBand.Y, false)
  /** @group Constructors */ case object J extends GpiFilter("J", "J", "J", MagnitudeBand.J, false)
  /** @group Constructors */ case object H extends GpiFilter("H", "H", "H", MagnitudeBand.H, false)
  /** @group Constructors */ case object K1 extends GpiFilter("K1", "K1", "K1", MagnitudeBand.K, false)
  /** @group Constructors */ case object K2 extends GpiFilter("K2", "K2", "K2", MagnitudeBand.K, false)

  /** All members of GpiFilter, in canonical order. */
  val all: List[GpiFilter] =
    List(Y, J, H, K1, K2)

  /** Select the member of GpiFilter with the given tag, if any. */
  def fromTag(s: String): Option[GpiFilter] =
    all.find(_.tag === s)

  /** Select the member of GpiFilter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiFilter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiFilterEnumerated: Enumerated[GpiFilter] =
    new Enumerated[GpiFilter] {
      def all = GpiFilter.all
      def tag(a: GpiFilter) = a.tag
      override def unsafeFromTag(s: String): GpiFilter =
        GpiFilter.unsafeFromTag(s)
    }

}