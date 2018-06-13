// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.math.Wavelength
import gem.util.Enumerated

/**
 * Enumerated type for magnitude band.
 * @group Enumerations (Generated)
 */
sealed abstract class MagnitudeBand(
  val tag: String,
  val shortName: String,
  val longName: String,
  val center: Wavelength,
  val width: Int,
  val magnitudeSystem: MagnitudeSystem
) extends Product with Serializable

object MagnitudeBand {

  /** @group Constructors */ case object SloanU extends MagnitudeBand("SloanU", "u", "UV", Wavelength.fromAngstroms.unsafeGet(3560), 46, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanG extends MagnitudeBand("SloanG", "g", "Green", Wavelength.fromAngstroms.unsafeGet(4830), 99, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanR extends MagnitudeBand("SloanR", "r", "Red", Wavelength.fromAngstroms.unsafeGet(6260), 96, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanI extends MagnitudeBand("SloanI", "i", "Far red", Wavelength.fromAngstroms.unsafeGet(7670), 106, MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanZ extends MagnitudeBand("SloanZ", "z", "Near infrared", Wavelength.fromAngstroms.unsafeGet(9100), 125, MagnitudeSystem.AB)
  /** @group Constructors */ case object U extends MagnitudeBand("U", "U", "Ultraviolet", Wavelength.fromAngstroms.unsafeGet(3600), 75, MagnitudeSystem.Vega)
  /** @group Constructors */ case object B extends MagnitudeBand("B", "B", "Blue", Wavelength.fromAngstroms.unsafeGet(4400), 90, MagnitudeSystem.Vega)
  /** @group Constructors */ case object V extends MagnitudeBand("V", "V", "Visual", Wavelength.fromAngstroms.unsafeGet(5500), 85, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Uc extends MagnitudeBand("Uc", "UC", "UCAC", Wavelength.fromAngstroms.unsafeGet(6100), 63, MagnitudeSystem.Vega)
  /** @group Constructors */ case object R extends MagnitudeBand("R", "R", "Red", Wavelength.fromAngstroms.unsafeGet(6700), 100, MagnitudeSystem.Vega)
  /** @group Constructors */ case object I extends MagnitudeBand("I", "I", "Infrared", Wavelength.fromAngstroms.unsafeGet(8700), 100, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Y extends MagnitudeBand("Y", "Y", "Y", Wavelength.fromAngstroms.unsafeGet(10200), 120, MagnitudeSystem.Vega)
  /** @group Constructors */ case object J extends MagnitudeBand("J", "J", "J", Wavelength.fromAngstroms.unsafeGet(12500), 240, MagnitudeSystem.Vega)
  /** @group Constructors */ case object H extends MagnitudeBand("H", "H", "H", Wavelength.fromAngstroms.unsafeGet(16500), 300, MagnitudeSystem.Vega)
  /** @group Constructors */ case object K extends MagnitudeBand("K", "K", "K", Wavelength.fromAngstroms.unsafeGet(22000), 410, MagnitudeSystem.Vega)
  /** @group Constructors */ case object L extends MagnitudeBand("L", "L", "L", Wavelength.fromAngstroms.unsafeGet(37600), 700, MagnitudeSystem.Vega)
  /** @group Constructors */ case object M extends MagnitudeBand("M", "M", "M", Wavelength.fromAngstroms.unsafeGet(47700), 240, MagnitudeSystem.Vega)
  /** @group Constructors */ case object N extends MagnitudeBand("N", "N", "N", Wavelength.fromAngstroms.unsafeGet(104700), 5230, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Q extends MagnitudeBand("Q", "Q", "Q", Wavelength.fromAngstroms.unsafeGet(201300), 1650, MagnitudeSystem.Vega)
  /** @group Constructors */ case object Ap extends MagnitudeBand("Ap", "AP", "Apparent", Wavelength.fromAngstroms.unsafeGet(5500), 85, MagnitudeSystem.Vega)

  /** All members of MagnitudeBand, in canonical order. */
  val all: List[MagnitudeBand] =
    List(SloanU, SloanG, SloanR, SloanI, SloanZ, U, B, V, Uc, R, I, Y, J, H, K, L, M, N, Q, Ap)

  /** Select the member of MagnitudeBand with the given tag, if any. */
  def fromTag(s: String): Option[MagnitudeBand] =
    all.find(_.tag === s)

  /** Select the member of MagnitudeBand with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): MagnitudeBand =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val MagnitudeBandEnumerated: Enumerated[MagnitudeBand] =
    new Enumerated[MagnitudeBand] {
      def all = MagnitudeBand.all
      def tag(a: MagnitudeBand) = a.tag
    }

}