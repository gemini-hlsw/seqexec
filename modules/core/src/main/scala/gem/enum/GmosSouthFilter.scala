// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import scalaz.syntax.equal._
import scalaz.std.string._

/**
 * Enumerated type for GMOS South filters.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: gem.math.Angle,
  val obsolete: Boolean
)

object GmosSouthFilter {

  /** @group Constructors */ case object UPrime extends GmosSouthFilter("UPrime", "u", "u_G0332", gem.math.Angle.fromDoubleArcseconds(0.35), false)
  /** @group Constructors */ case object GPrime extends GmosSouthFilter("GPrime", "g", "g_G0325", gem.math.Angle.fromDoubleArcseconds(0.475), false)
  /** @group Constructors */ case object RPrime extends GmosSouthFilter("RPrime", "r", "r_G0326", gem.math.Angle.fromDoubleArcseconds(0.63), false)
  /** @group Constructors */ case object IPrime extends GmosSouthFilter("IPrime", "i", "i_G0327", gem.math.Angle.fromDoubleArcseconds(0.78), false)
  /** @group Constructors */ case object ZPrime extends GmosSouthFilter("ZPrime", "z", "z_G0328", gem.math.Angle.fromDoubleArcseconds(0.925), false)
  /** @group Constructors */ case object Z extends GmosSouthFilter("Z", "Z", "Z_G0343", gem.math.Angle.fromDoubleArcseconds(0.876), false)
  /** @group Constructors */ case object Y extends GmosSouthFilter("Y", "Y", "Y_G0344", gem.math.Angle.fromDoubleArcseconds(1.01), false)
  /** @group Constructors */ case object GG455 extends GmosSouthFilter("GG455", "GG455", "GG455_G0329", gem.math.Angle.fromDoubleArcseconds(0.68), false)
  /** @group Constructors */ case object OG515 extends GmosSouthFilter("OG515", "OG515", "OG515_G0330", gem.math.Angle.fromDoubleArcseconds(0.71), false)
  /** @group Constructors */ case object RG610 extends GmosSouthFilter("RG610", "RG610", "RG610_G0331", gem.math.Angle.fromDoubleArcseconds(0.75), false)
  /** @group Constructors */ case object RG780 extends GmosSouthFilter("RG780", "RG780", "RG780_G0334", gem.math.Angle.fromDoubleArcseconds(0.85), false)
  /** @group Constructors */ case object CaT extends GmosSouthFilter("CaT", "CaT", "CaT_G0333", gem.math.Angle.fromDoubleArcseconds(0.86), false)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosSouthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0337 + r_G0326", gem.math.Angle.fromDoubleArcseconds(0.63), false)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosSouthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0338 + r_G0326", gem.math.Angle.fromDoubleArcseconds(0.63), false)
  /** @group Constructors */ case object GPrime_GG455 extends GmosSouthFilter("GPrime_GG455", "g+GG455", "g_G0325 + GG455_G0329", gem.math.Angle.fromDoubleArcseconds(0.506), false)
  /** @group Constructors */ case object GPrime_OG515 extends GmosSouthFilter("GPrime_OG515", "g+OG515", "g_G0325 + OG515_G0330", gem.math.Angle.fromDoubleArcseconds(0.536), false)
  /** @group Constructors */ case object RPrime_RG610 extends GmosSouthFilter("RPrime_RG610", "r+RG610", "r_G0326 + RG610_G0331", gem.math.Angle.fromDoubleArcseconds(0.657), false)
  /** @group Constructors */ case object IPrime_RG780 extends GmosSouthFilter("IPrime_RG780", "i+RG780", "i_G0327 + RG780_G0334", gem.math.Angle.fromDoubleArcseconds(0.819), false)
  /** @group Constructors */ case object IPrime_CaT extends GmosSouthFilter("IPrime_CaT", "i+CaT", "i_G0327 + CaT_G0333", gem.math.Angle.fromDoubleArcseconds(0.815), false)
  /** @group Constructors */ case object ZPrime_CaT extends GmosSouthFilter("ZPrime_CaT", "z+Cat", "z_G0328 + CaT_G0333", gem.math.Angle.fromDoubleArcseconds(0.89), false)
  /** @group Constructors */ case object Ha extends GmosSouthFilter("Ha", "Ha", "Ha_G0336", gem.math.Angle.fromDoubleArcseconds(0.656), false)
  /** @group Constructors */ case object SII extends GmosSouthFilter("SII", "SII", "SII_G0335", gem.math.Angle.fromDoubleArcseconds(0.672), false)
  /** @group Constructors */ case object HaC extends GmosSouthFilter("HaC", "HaC", "HaC_G0337", gem.math.Angle.fromDoubleArcseconds(0.662), false)
  /** @group Constructors */ case object OIII extends GmosSouthFilter("OIII", "OIII", "OIII_G0338", gem.math.Angle.fromDoubleArcseconds(0.499), false)
  /** @group Constructors */ case object OIIIC extends GmosSouthFilter("OIIIC", "OIIIC", "OIIIC_G0339", gem.math.Angle.fromDoubleArcseconds(0.514), false)
  /** @group Constructors */ case object HeII extends GmosSouthFilter("HeII", "HeII", "HeII_G0340", gem.math.Angle.fromDoubleArcseconds(0.468), false)
  /** @group Constructors */ case object HeIIC extends GmosSouthFilter("HeIIC", "HeIIC", "HeIIC_G0341", gem.math.Angle.fromDoubleArcseconds(0.478), false)
  /** @group Constructors */ case object Lya395 extends GmosSouthFilter("Lya395", "Lya395", "Lya395_G0342", gem.math.Angle.fromDoubleArcseconds(0.396), false)

  /** All members of GmosSouthFilter, in canonical order. */
  val all: List[GmosSouthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime, Z, Y, GG455, OG515, RG610, RG780, CaT, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_RG780, IPrime_CaT, ZPrime_CaT, Ha, SII, HaC, OIII, OIIIC, HeII, HeIIC, Lya395)

  /** Select the member of GmosSouthFilter with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthFilter] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthFilter with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosSouthFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosSouthFilterEnumerated: Enumerated[GmosSouthFilter] =
    new Enumerated[GmosSouthFilter] {
      def all = GmosSouthFilter.all
      def tag(a: GmosSouthFilter) = a.tag
    }

}