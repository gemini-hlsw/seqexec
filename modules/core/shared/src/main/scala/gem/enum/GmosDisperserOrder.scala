// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS disperser order.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosDisperserOrder(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int
) extends Product with Serializable

object GmosDisperserOrder {

  /** @group Constructors */ case object Zero extends GmosDisperserOrder("Zero", "0", "Zero", 0)
  /** @group Constructors */ case object One extends GmosDisperserOrder("One", "1", "One", 1)
  /** @group Constructors */ case object Two extends GmosDisperserOrder("Two", "2", "Two", 2)

  /** All members of GmosDisperserOrder, in canonical order. */
  val all: List[GmosDisperserOrder] =
    List(Zero, One, Two)

  /** Select the member of GmosDisperserOrder with the given tag, if any. */
  def fromTag(s: String): Option[GmosDisperserOrder] =
    all.find(_.tag === s)

  /** Select the member of GmosDisperserOrder with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosDisperserOrder =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosDisperserOrderEnumerated: Enumerated[GmosDisperserOrder] =
    new Enumerated[GmosDisperserOrder] {
      def all = GmosDisperserOrder.all
      def tag(a: GmosDisperserOrder) = a.tag
    }

}