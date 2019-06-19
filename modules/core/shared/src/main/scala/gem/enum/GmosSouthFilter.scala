// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import gsp.math.Wavelength

/**
 * Enumerated type for GMOS South filters.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val obsolete: Boolean
) extends Product with Serializable

object GmosSouthFilter {

  /** @group Constructors */ case object UPrime extends GmosSouthFilter("UPrime", "u", "u_G0332", Wavelength.fromPicometers.unsafeGet(350000), false)
  /** @group Constructors */ case object GPrime extends GmosSouthFilter("GPrime", "g", "g_G0325", Wavelength.fromPicometers.unsafeGet(475000), false)
  /** @group Constructors */ case object RPrime extends GmosSouthFilter("RPrime", "r", "r_G0326", Wavelength.fromPicometers.unsafeGet(630000), false)
  /** @group Constructors */ case object IPrime extends GmosSouthFilter("IPrime", "i", "i_G0327", Wavelength.fromPicometers.unsafeGet(780000), false)
  /** @group Constructors */ case object ZPrime extends GmosSouthFilter("ZPrime", "z", "z_G0328", Wavelength.fromPicometers.unsafeGet(925000), false)
  /** @group Constructors */ case object Z extends GmosSouthFilter("Z", "Z", "Z_G0343", Wavelength.fromPicometers.unsafeGet(876000), false)
  /** @group Constructors */ case object Y extends GmosSouthFilter("Y", "Y", "Y_G0344", Wavelength.fromPicometers.unsafeGet(1010000), false)
  /** @group Constructors */ case object GG455 extends GmosSouthFilter("GG455", "GG455", "GG455_G0329", Wavelength.fromPicometers.unsafeGet(680000), false)
  /** @group Constructors */ case object OG515 extends GmosSouthFilter("OG515", "OG515", "OG515_G0330", Wavelength.fromPicometers.unsafeGet(710000), false)
  /** @group Constructors */ case object RG610 extends GmosSouthFilter("RG610", "RG610", "RG610_G0331", Wavelength.fromPicometers.unsafeGet(750000), false)
  /** @group Constructors */ case object RG780 extends GmosSouthFilter("RG780", "RG780", "RG780_G0334", Wavelength.fromPicometers.unsafeGet(850000), false)
  /** @group Constructors */ case object CaT extends GmosSouthFilter("CaT", "CaT", "CaT_G0333", Wavelength.fromPicometers.unsafeGet(860000), false)
  /** @group Constructors */ case object HartmannA_RPrime extends GmosSouthFilter("HartmannA_RPrime", "r+HartA", "HartmannA_G0337 + r_G0326", Wavelength.fromPicometers.unsafeGet(630000), false)
  /** @group Constructors */ case object HartmannB_RPrime extends GmosSouthFilter("HartmannB_RPrime", "r+HartB", "HartmannB_G0338 + r_G0326", Wavelength.fromPicometers.unsafeGet(630000), false)
  /** @group Constructors */ case object GPrime_GG455 extends GmosSouthFilter("GPrime_GG455", "g+GG455", "g_G0325 + GG455_G0329", Wavelength.fromPicometers.unsafeGet(506000), false)
  /** @group Constructors */ case object GPrime_OG515 extends GmosSouthFilter("GPrime_OG515", "g+OG515", "g_G0325 + OG515_G0330", Wavelength.fromPicometers.unsafeGet(536000), false)
  /** @group Constructors */ case object RPrime_RG610 extends GmosSouthFilter("RPrime_RG610", "r+RG610", "r_G0326 + RG610_G0331", Wavelength.fromPicometers.unsafeGet(657000), false)
  /** @group Constructors */ case object IPrime_RG780 extends GmosSouthFilter("IPrime_RG780", "i+RG780", "i_G0327 + RG780_G0334", Wavelength.fromPicometers.unsafeGet(819000), false)
  /** @group Constructors */ case object IPrime_CaT extends GmosSouthFilter("IPrime_CaT", "i+CaT", "i_G0327 + CaT_G0333", Wavelength.fromPicometers.unsafeGet(815000), false)
  /** @group Constructors */ case object ZPrime_CaT extends GmosSouthFilter("ZPrime_CaT", "z+Cat", "z_G0328 + CaT_G0333", Wavelength.fromPicometers.unsafeGet(890000), false)
  /** @group Constructors */ case object Ha extends GmosSouthFilter("Ha", "Ha", "Ha_G0336", Wavelength.fromPicometers.unsafeGet(656000), false)
  /** @group Constructors */ case object SII extends GmosSouthFilter("SII", "SII", "SII_G0335", Wavelength.fromPicometers.unsafeGet(672000), false)
  /** @group Constructors */ case object HaC extends GmosSouthFilter("HaC", "HaC", "HaC_G0337", Wavelength.fromPicometers.unsafeGet(662000), false)
  /** @group Constructors */ case object OIII extends GmosSouthFilter("OIII", "OIII", "OIII_G0338", Wavelength.fromPicometers.unsafeGet(499000), false)
  /** @group Constructors */ case object OIIIC extends GmosSouthFilter("OIIIC", "OIIIC", "OIIIC_G0339", Wavelength.fromPicometers.unsafeGet(514000), false)
  /** @group Constructors */ case object HeII extends GmosSouthFilter("HeII", "HeII", "HeII_G0340", Wavelength.fromPicometers.unsafeGet(468000), false)
  /** @group Constructors */ case object HeIIC extends GmosSouthFilter("HeIIC", "HeIIC", "HeIIC_G0341", Wavelength.fromPicometers.unsafeGet(478000), false)
  /** @group Constructors */ case object Lya395 extends GmosSouthFilter("Lya395", "Lya395", "Lya395_G0342", Wavelength.fromPicometers.unsafeGet(396000), false)

  /** All members of GmosSouthFilter, in canonical order. */
  val all: List[GmosSouthFilter] =
    List(UPrime, GPrime, RPrime, IPrime, ZPrime, Z, Y, GG455, OG515, RG610, RG780, CaT, HartmannA_RPrime, HartmannB_RPrime, GPrime_GG455, GPrime_OG515, RPrime_RG610, IPrime_RG780, IPrime_CaT, ZPrime_CaT, Ha, SII, HaC, OIII, OIIIC, HeII, HeIIC, Lya395)

  /** Select the member of GmosSouthFilter with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthFilter] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthFilter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GmosSouthFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GmosSouthFilter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GmosSouthFilterEnumerated: Enumerated[GmosSouthFilter] =
    new Enumerated[GmosSouthFilter] {
      def all = GmosSouthFilter.all
      def tag(a: GmosSouthFilter) = a.tag
      override def unsafeFromTag(s: String): GmosSouthFilter =
        GmosSouthFilter.unsafeFromTag(s)
    }

}