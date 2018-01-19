// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.math.Wavelength
import gem.util.Enumerated

/**
 * Enumerated type for GNRIS Disperser Order.
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

  /** @group Constructors */ case object One extends GnirsDisperserOrder("One", "1", "One", 1, Wavelength.unsafeFromAngstroms(48500), Wavelength.unsafeFromAngstroms(43000), Wavelength.unsafeFromAngstroms(60000), Wavelength.unsafeFromAngstroms(0), Some(MagnitudeBand.M), false)
  /** @group Constructors */ case object Two extends GnirsDisperserOrder("Two", "2", "Two", 2, Wavelength.unsafeFromAngstroms(34000), Wavelength.unsafeFromAngstroms(27000), Wavelength.unsafeFromAngstroms(43000), Wavelength.unsafeFromAngstroms(0), Some(MagnitudeBand.L), false)
  /** @group Constructors */ case object Three extends GnirsDisperserOrder("Three", "3", "Three", 3, Wavelength.unsafeFromAngstroms(22200), Wavelength.unsafeFromAngstroms(18600), Wavelength.unsafeFromAngstroms(27000), Wavelength.unsafeFromAngstroms(6), Some(MagnitudeBand.K), true)
  /** @group Constructors */ case object FourXD extends GnirsDisperserOrder("FourXD", "4XD", "FourXD", 4, Wavelength.unsafeFromAngstroms(16500), Wavelength.unsafeFromAngstroms(14200), Wavelength.unsafeFromAngstroms(18600), Wavelength.unsafeFromAngstroms(5), Some(MagnitudeBand.H), true)
  /** @group Constructors */ case object Four extends GnirsDisperserOrder("Four", "4", "Four", 4, Wavelength.unsafeFromAngstroms(16300), Wavelength.unsafeFromAngstroms(14200), Wavelength.unsafeFromAngstroms(18600), Wavelength.unsafeFromAngstroms(5), Some(MagnitudeBand.H), true)
  /** @group Constructors */ case object Five extends GnirsDisperserOrder("Five", "5", "Five", 5, Wavelength.unsafeFromAngstroms(12500), Wavelength.unsafeFromAngstroms(11700), Wavelength.unsafeFromAngstroms(14200), Wavelength.unsafeFromAngstroms(4), Some(MagnitudeBand.J), true)
  /** @group Constructors */ case object Six extends GnirsDisperserOrder("Six", "6", "Six", 6, Wavelength.unsafeFromAngstroms(11000), Wavelength.unsafeFromAngstroms(10300), Wavelength.unsafeFromAngstroms(11700), Wavelength.unsafeFromAngstroms(3), None, true)
  /** @group Constructors */ case object Seven extends GnirsDisperserOrder("Seven", "7", "Seven", 7, Wavelength.unsafeFromAngstroms(9510), Wavelength.unsafeFromAngstroms(8800), Wavelength.unsafeFromAngstroms(10300), Wavelength.unsafeFromAngstroms(3), None, true)
  /** @group Constructors */ case object Eight extends GnirsDisperserOrder("Eight", "8", "Eight", 8, Wavelength.unsafeFromAngstroms(8320), Wavelength.unsafeFromAngstroms(7800), Wavelength.unsafeFromAngstroms(8800), Wavelength.unsafeFromAngstroms(2), None, true)

  /** All members of GnirsDisperserOrder, in canonical order. */
  val all: List[GnirsDisperserOrder] =
    List(One, Two, Three, FourXD, Four, Five, Six, Seven, Eight)

  /** Select the member of GnirsDisperserOrder with the given tag, if any. */
  def fromTag(s: String): Option[GnirsDisperserOrder] =
    all.find(_.tag === s)

  /** Select the member of GnirsDisperserOrder with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GnirsDisperserOrder =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GnirsDisperserOrderEnumerated: Enumerated[GnirsDisperserOrder] =
    new Enumerated[GnirsDisperserOrder] {
      def all = GnirsDisperserOrder.all
      def tag(a: GnirsDisperserOrder) = a.tag
    }

}