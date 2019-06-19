// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import gsp.math.Wavelength

/**
 * Enumerated type for GNIRS Disperser Order.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsDisperserOrder(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int,
  val defaultWavelength: Wavelength,
  val minWavelength: Wavelength,
  val maxWavelength: Wavelength,
  val deltaWavelength: Wavelength,
  val band: Option[MagnitudeBand],
  val cross_dispersed: Boolean
) extends Product with Serializable

object GnirsDisperserOrder {

  /** @group Constructors */ case object One extends GnirsDisperserOrder("One", "1", "One", 1, Wavelength.fromPicometers.unsafeGet(4850000), Wavelength.fromPicometers.unsafeGet(4300000), Wavelength.fromPicometers.unsafeGet(6000000), Wavelength.fromPicometers.unsafeGet(0), Some(MagnitudeBand.M), false)
  /** @group Constructors */ case object Two extends GnirsDisperserOrder("Two", "2", "Two", 2, Wavelength.fromPicometers.unsafeGet(3400000), Wavelength.fromPicometers.unsafeGet(2700000), Wavelength.fromPicometers.unsafeGet(4300000), Wavelength.fromPicometers.unsafeGet(0), Some(MagnitudeBand.L), false)
  /** @group Constructors */ case object Three extends GnirsDisperserOrder("Three", "3", "Three", 3, Wavelength.fromPicometers.unsafeGet(2220000), Wavelength.fromPicometers.unsafeGet(1860000), Wavelength.fromPicometers.unsafeGet(2700000), Wavelength.fromPicometers.unsafeGet(647), Some(MagnitudeBand.K), true)
  /** @group Constructors */ case object FourXD extends GnirsDisperserOrder("FourXD", "4XD", "FourXD", 4, Wavelength.fromPicometers.unsafeGet(1650000), Wavelength.fromPicometers.unsafeGet(1420000), Wavelength.fromPicometers.unsafeGet(1860000), Wavelength.fromPicometers.unsafeGet(482), Some(MagnitudeBand.H), true)
  /** @group Constructors */ case object Four extends GnirsDisperserOrder("Four", "4", "Four", 4, Wavelength.fromPicometers.unsafeGet(1630000), Wavelength.fromPicometers.unsafeGet(1420000), Wavelength.fromPicometers.unsafeGet(1860000), Wavelength.fromPicometers.unsafeGet(485), Some(MagnitudeBand.H), true)
  /** @group Constructors */ case object Five extends GnirsDisperserOrder("Five", "5", "Five", 5, Wavelength.fromPicometers.unsafeGet(1250000), Wavelength.fromPicometers.unsafeGet(1170000), Wavelength.fromPicometers.unsafeGet(1420000), Wavelength.fromPicometers.unsafeGet(388), Some(MagnitudeBand.J), true)
  /** @group Constructors */ case object Six extends GnirsDisperserOrder("Six", "6", "Six", 6, Wavelength.fromPicometers.unsafeGet(1100000), Wavelength.fromPicometers.unsafeGet(1030000), Wavelength.fromPicometers.unsafeGet(1170000), Wavelength.fromPicometers.unsafeGet(323), None, true)
  /** @group Constructors */ case object Seven extends GnirsDisperserOrder("Seven", "7", "Seven", 7, Wavelength.fromPicometers.unsafeGet(951000), Wavelength.fromPicometers.unsafeGet(880000), Wavelength.fromPicometers.unsafeGet(1030000), Wavelength.fromPicometers.unsafeGet(276), None, true)
  /** @group Constructors */ case object Eight extends GnirsDisperserOrder("Eight", "8", "Eight", 8, Wavelength.fromPicometers.unsafeGet(832000), Wavelength.fromPicometers.unsafeGet(780000), Wavelength.fromPicometers.unsafeGet(880000), Wavelength.fromPicometers.unsafeGet(241), None, true)

  /** All members of GnirsDisperserOrder, in canonical order. */
  val all: List[GnirsDisperserOrder] =
    List(One, Two, Three, FourXD, Four, Five, Six, Seven, Eight)

  /** Select the member of GnirsDisperserOrder with the given tag, if any. */
  def fromTag(s: String): Option[GnirsDisperserOrder] =
    all.find(_.tag === s)

  /** Select the member of GnirsDisperserOrder with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsDisperserOrder =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GnirsDisperserOrder: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GnirsDisperserOrderEnumerated: Enumerated[GnirsDisperserOrder] =
    new Enumerated[GnirsDisperserOrder] {
      def all = GnirsDisperserOrder.all
      def tag(a: GnirsDisperserOrder) = a.tag
      override def unsafeFromTag(s: String): GnirsDisperserOrder =
        GnirsDisperserOrder.unsafeFromTag(s)
    }

}