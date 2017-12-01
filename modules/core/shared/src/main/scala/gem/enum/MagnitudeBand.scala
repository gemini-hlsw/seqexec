// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for magnitude band.
 * @group Enumerations (Generated)
 */
sealed abstract class MagnitudeBand(
  val tag: String,
  val shortName: String,
  val longName: String,
  val center: Int,
  val width: Int,
  val magnitudeSystem: gem.enum.MagnitudeSystem
) extends Product with Serializable

object MagnitudeBand {

  /** @group Constructors */ case object SloanU extends MagnitudeBand("SloanU", "u", "UV", 356, 46, gem.enum.MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanG extends MagnitudeBand("SloanG", "g", "Green", 483, 99, gem.enum.MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanR extends MagnitudeBand("SloanR", "r", "Red", 626, 96, gem.enum.MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanI extends MagnitudeBand("SloanI", "i", "Far red", 767, 106, gem.enum.MagnitudeSystem.AB)
  /** @group Constructors */ case object SloanZ extends MagnitudeBand("SloanZ", "z", "Near infrared", 910, 125, gem.enum.MagnitudeSystem.AB)
  /** @group Constructors */ case object U extends MagnitudeBand("U", "U", "Ultraviolet", 360, 75, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object B extends MagnitudeBand("B", "B", "Blue", 440, 90, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object V extends MagnitudeBand("V", "V", "Visual", 550, 85, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object Uc extends MagnitudeBand("Uc", "UC", "UCAC", 610, 63, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object R extends MagnitudeBand("R", "R", "Red", 670, 100, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object I extends MagnitudeBand("I", "I", "Infrared", 870, 100, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object Y extends MagnitudeBand("Y", "Y", "Y", 1020, 120, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object J extends MagnitudeBand("J", "J", "J", 1250, 240, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object H extends MagnitudeBand("H", "H", "H", 1650, 300, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object K extends MagnitudeBand("K", "K", "K", 2200, 410, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object L extends MagnitudeBand("L", "L", "L", 3760, 700, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object M extends MagnitudeBand("M", "M", "M", 4770, 240, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object N extends MagnitudeBand("N", "N", "N", 10470, 5230, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object Q extends MagnitudeBand("Q", "Q", "Q", 20130, 1650, gem.enum.MagnitudeSystem.Vega)
  /** @group Constructors */ case object Ap extends MagnitudeBand("Ap", "AP", "Apparent", 550, 85, gem.enum.MagnitudeSystem.Vega)

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