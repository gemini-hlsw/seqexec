// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GSAOI Filter.
 * @group Enumerations (Generated)
 */
sealed abstract class GsaoiFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Double,
  val readMode: GsaoiReadMode,
  val expsoureTime5050: Double,
  val exposureTimeHalfWell: Double,
  val band: Option[MagnitudeBand]
) extends Product with Serializable

object GsaoiFilter {

  /** @group Constructors */ case object Z extends GsaoiFilter("Z", "Z", "Z (1.015 um)", 1.02, GsaoiReadMode.Faint, 26.0, 4619.0, Some(MagnitudeBand.J))
  /** @group Constructors */ case object HeI extends GsaoiFilter("HeI", "HeI", "HeI (1.083 um)", 1.08, GsaoiReadMode.VeryFaint, 72.6, 21792.0, Some(MagnitudeBand.J))
  /** @group Constructors */ case object PaGamma extends GsaoiFilter("PaGamma", "Pagma", "Pa(gamma) (1.094 um)", 1.09, GsaoiReadMode.VeryFaint, 122.0, 36585.0, Some(MagnitudeBand.J))
  /** @group Constructors */ case object JContinuum extends GsaoiFilter("JContinuum", "Jcont", "J-continuum (1.207 um)", 1.21, GsaoiReadMode.VeryFaint, 32.6, 9793.0, Some(MagnitudeBand.J))
  /** @group Constructors */ case object J extends GsaoiFilter("J", "J", "J (1.250 um)", 1.25, GsaoiReadMode.Faint, 5.7, 1004.0, Some(MagnitudeBand.J))
  /** @group Constructors */ case object H extends GsaoiFilter("H", "H", "H (1.635 um)", 1.64, GsaoiReadMode.Bright, 12.0, 460.0, Some(MagnitudeBand.H))
  /** @group Constructors */ case object PaBeta extends GsaoiFilter("PaBeta", "Pabeta", "Pa(beta) (1.282 um)", 1.28, GsaoiReadMode.Faint, 21.8, 3879.0, Some(MagnitudeBand.J))
  /** @group Constructors */ case object HContinuum extends GsaoiFilter("HContinuum", "Hcont", "H-continuum (1.570 um)", 1.57, GsaoiReadMode.Faint, 31.2, 5545.0, Some(MagnitudeBand.H))
  /** @group Constructors */ case object CH4Short extends GsaoiFilter("CH4Short", "CH4short", "CH4(short) (1.580 um)", 1.58, GsaoiReadMode.Faint, 6.6, 1174.0, Some(MagnitudeBand.H))
  /** @group Constructors */ case object FeII extends GsaoiFilter("FeII", "FeII1644", "[Fe II] (1.644 um)", 1.64, GsaoiReadMode.Faint, 24.9, 4416.0, Some(MagnitudeBand.H))
  /** @group Constructors */ case object CH4Long extends GsaoiFilter("CH4Long", "CH4long", "CH4(long) (1.690 um)", 1.69, GsaoiReadMode.Faint, 6.8, 1202.0, Some(MagnitudeBand.H))
  /** @group Constructors */ case object H20Ice extends GsaoiFilter("H20Ice", "H20ice", "H20 ice (2.000 um)", 2.0, GsaoiReadMode.Faint, 19.1, 3395.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object HeI2p2s extends GsaoiFilter("HeI2p2s", "HeI2p2s", "HeI (2p2s) (2.058 um)", 2.06, GsaoiReadMode.Faint, 28.3, 5032.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KContinuum1 extends GsaoiFilter("KContinuum1", "Kcontshrt", "Ks-continuum (2.093 um)", 2.09, GsaoiReadMode.Faint, 7.8, 6069.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object BrGamma extends GsaoiFilter("BrGamma", "Brgma", "Br(gamma) (2.166 um)", 2.17, GsaoiReadMode.Faint, 31.0, 5496.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KContinuum2 extends GsaoiFilter("KContinuum2", "Kcontlong", "Kl-continuum (2.270 um)", 2.27, GsaoiReadMode.Faint, 33.3, 5911.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KPrime extends GsaoiFilter("KPrime", "Kprime", "K(prime) (2.120 um)", 2.12, GsaoiReadMode.Bright, 14.8, 566.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object H2_1_0_S_1 extends GsaoiFilter("H2_1_0_S_1", "H2(1-0)", "H2 1-0 S(1) (2.122 um)", 2.12, GsaoiReadMode.Faint, 27.5, 5400.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KShort extends GsaoiFilter("KShort", "Kshort", "K(short) (2.150 um)", 2.15, GsaoiReadMode.Bright, 14.4, 551.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object K extends GsaoiFilter("K", "K", "K (2.200 um)", 2.2, GsaoiReadMode.Bright, 12.3, 470.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object H2_2_1_S_1 extends GsaoiFilter("H2_2_1_S_1", "H2(2-1)", "H2 2-1 S(1) (2.248 um)", 2.25, GsaoiReadMode.Faint, 32.6, 5784.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object CO extends GsaoiFilter("CO", "CO2360", "CO (2.360 um)", 2.36, GsaoiReadMode.Faint, 7.7, 1370.0, Some(MagnitudeBand.K))
  /** @group Constructors */ case object Diffuser1 extends GsaoiFilter("Diffuser1", "Diffuser1", "Diffuser1", 0.0, GsaoiReadMode.Bright, 0.0, 0.0, Option.empty[MagnitudeBand])
  /** @group Constructors */ case object Diffuser2 extends GsaoiFilter("Diffuser2", "Diffuser2", "Diffuser2", 0.0, GsaoiReadMode.Bright, 0.0, 0.0, Option.empty[MagnitudeBand])
  /** @group Constructors */ case object Blocked extends GsaoiFilter("Blocked", "Blocked", "Blocked", 0.0, GsaoiReadMode.Bright, 0.0, 0.0, Option.empty[MagnitudeBand])

  /** All members of GsaoiFilter, in canonical order. */
  val all: List[GsaoiFilter] =
    List(Z, HeI, PaGamma, JContinuum, J, H, PaBeta, HContinuum, CH4Short, FeII, CH4Long, H20Ice, HeI2p2s, KContinuum1, BrGamma, KContinuum2, KPrime, H2_1_0_S_1, KShort, K, H2_2_1_S_1, CO, Diffuser1, Diffuser2, Blocked)

  /** Select the member of GsaoiFilter with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiFilter] =
    all.find(_.tag === s)

  /** Select the member of GsaoiFilter with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GsaoiFilter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GsaoiFilter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GsaoiFilterEnumerated: Enumerated[GsaoiFilter] =
    new Enumerated[GsaoiFilter] {
      def all = GsaoiFilter.all
      def tag(a: GsaoiFilter) = a.tag
      override def unsafeFromTag(s: String): GsaoiFilter =
        GsaoiFilter.unsafeFromTag(s)
    }

}