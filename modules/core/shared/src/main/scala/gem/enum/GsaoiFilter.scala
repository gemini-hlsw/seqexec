// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import gsp.math.Wavelength
import scala.concurrent.duration._

/**
 * Enumerated type for GSAOI Filter.
 * @group Enumerations (Generated)
 */
sealed abstract class GsaoiFilter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val wavelength: Wavelength,
  val readMode: GsaoiReadMode,
  val exposureTime5050: FiniteDuration,
  val exposureTimeHalfWell: FiniteDuration,
  val band: Option[MagnitudeBand]
) extends Product with Serializable

object GsaoiFilter {

  /** @group Constructors */ case object Z extends GsaoiFilter("Z", "Z", "Z (1.015 um)", Wavelength.fromPicometers.unsafeGet(1020000), GsaoiReadMode.Faint, 26000.millis, 4619000.millis, Some(MagnitudeBand.J))
  /** @group Constructors */ case object HeI extends GsaoiFilter("HeI", "HeI", "HeI (1.083 um)", Wavelength.fromPicometers.unsafeGet(1080000), GsaoiReadMode.VeryFaint, 72600.millis, 21792000.millis, Some(MagnitudeBand.J))
  /** @group Constructors */ case object PaGamma extends GsaoiFilter("PaGamma", "Pagma", "Pa(gamma) (1.094 um)", Wavelength.fromPicometers.unsafeGet(1090000), GsaoiReadMode.VeryFaint, 122000.millis, 36585000.millis, Some(MagnitudeBand.J))
  /** @group Constructors */ case object JContinuum extends GsaoiFilter("JContinuum", "Jcont", "J-continuum (1.207 um)", Wavelength.fromPicometers.unsafeGet(1210000), GsaoiReadMode.VeryFaint, 32600.millis, 9793000.millis, Some(MagnitudeBand.J))
  /** @group Constructors */ case object J extends GsaoiFilter("J", "J", "J (1.250 um)", Wavelength.fromPicometers.unsafeGet(1250000), GsaoiReadMode.Faint, 5700.millis, 1004000.millis, Some(MagnitudeBand.J))
  /** @group Constructors */ case object H extends GsaoiFilter("H", "H", "H (1.635 um)", Wavelength.fromPicometers.unsafeGet(1640000), GsaoiReadMode.Bright, 12000.millis, 460000.millis, Some(MagnitudeBand.H))
  /** @group Constructors */ case object PaBeta extends GsaoiFilter("PaBeta", "Pabeta", "Pa(beta) (1.282 um)", Wavelength.fromPicometers.unsafeGet(1280000), GsaoiReadMode.Faint, 21800.millis, 3879000.millis, Some(MagnitudeBand.J))
  /** @group Constructors */ case object HContinuum extends GsaoiFilter("HContinuum", "Hcont", "H-continuum (1.570 um)", Wavelength.fromPicometers.unsafeGet(1570000), GsaoiReadMode.Faint, 31200.millis, 5545000.millis, Some(MagnitudeBand.H))
  /** @group Constructors */ case object CH4Short extends GsaoiFilter("CH4Short", "CH4short", "CH4(short) (1.580 um)", Wavelength.fromPicometers.unsafeGet(1580000), GsaoiReadMode.Faint, 6600.millis, 1174000.millis, Some(MagnitudeBand.H))
  /** @group Constructors */ case object FeII extends GsaoiFilter("FeII", "FeII1644", "[Fe II] (1.644 um)", Wavelength.fromPicometers.unsafeGet(1640000), GsaoiReadMode.Faint, 24900.millis, 4416000.millis, Some(MagnitudeBand.H))
  /** @group Constructors */ case object CH4Long extends GsaoiFilter("CH4Long", "CH4long", "CH4(long) (1.690 um)", Wavelength.fromPicometers.unsafeGet(1690000), GsaoiReadMode.Faint, 6800.millis, 1202000.millis, Some(MagnitudeBand.H))
  /** @group Constructors */ case object H20Ice extends GsaoiFilter("H20Ice", "H20ice", "H20 ice (2.000 um)", Wavelength.fromPicometers.unsafeGet(2000000), GsaoiReadMode.Faint, 19100.millis, 3395000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object HeI2p2s extends GsaoiFilter("HeI2p2s", "HeI2p2s", "HeI (2p2s) (2.058 um)", Wavelength.fromPicometers.unsafeGet(2060000), GsaoiReadMode.Faint, 28300.millis, 5032000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KContinuum1 extends GsaoiFilter("KContinuum1", "Kcontshrt", "Ks-continuum (2.093 um)", Wavelength.fromPicometers.unsafeGet(2090000), GsaoiReadMode.Faint, 7800.millis, 6069000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object BrGamma extends GsaoiFilter("BrGamma", "Brgma", "Br(gamma) (2.166 um)", Wavelength.fromPicometers.unsafeGet(2170000), GsaoiReadMode.Faint, 31000.millis, 5496000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KContinuum2 extends GsaoiFilter("KContinuum2", "Kcontlong", "Kl-continuum (2.270 um)", Wavelength.fromPicometers.unsafeGet(2270000), GsaoiReadMode.Faint, 33300.millis, 5911000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KPrime extends GsaoiFilter("KPrime", "Kprime", "K(prime) (2.120 um)", Wavelength.fromPicometers.unsafeGet(2120000), GsaoiReadMode.Bright, 14800.millis, 566000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object H2_1_0_S_1 extends GsaoiFilter("H2_1_0_S_1", "H2(1-0)", "H2 1-0 S(1) (2.122 um)", Wavelength.fromPicometers.unsafeGet(2120000), GsaoiReadMode.Faint, 27500.millis, 5400000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object KShort extends GsaoiFilter("KShort", "Kshort", "K(short) (2.150 um)", Wavelength.fromPicometers.unsafeGet(2150000), GsaoiReadMode.Bright, 14400.millis, 551000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object K extends GsaoiFilter("K", "K", "K (2.200 um)", Wavelength.fromPicometers.unsafeGet(2200000), GsaoiReadMode.Bright, 12300.millis, 470000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object H2_2_1_S_1 extends GsaoiFilter("H2_2_1_S_1", "H2(2-1)", "H2 2-1 S(1) (2.248 um)", Wavelength.fromPicometers.unsafeGet(2250000), GsaoiReadMode.Faint, 32600.millis, 5784000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object CO extends GsaoiFilter("CO", "CO2360", "CO (2.360 um)", Wavelength.fromPicometers.unsafeGet(2360000), GsaoiReadMode.Faint, 7700.millis, 1370000.millis, Some(MagnitudeBand.K))
  /** @group Constructors */ case object Diffuser1 extends GsaoiFilter("Diffuser1", "Diffuser1", "Diffuser1", Wavelength.fromPicometers.unsafeGet(0), GsaoiReadMode.Bright, 0.millis, 0.millis, Option.empty[MagnitudeBand])
  /** @group Constructors */ case object Diffuser2 extends GsaoiFilter("Diffuser2", "Diffuser2", "Diffuser2", Wavelength.fromPicometers.unsafeGet(0), GsaoiReadMode.Bright, 0.millis, 0.millis, Option.empty[MagnitudeBand])
  /** @group Constructors */ case object Blocked extends GsaoiFilter("Blocked", "Blocked", "Blocked", Wavelength.fromPicometers.unsafeGet(0), GsaoiReadMode.Bright, 0.millis, 0.millis, Option.empty[MagnitudeBand])

  /** All members of GsaoiFilter, in canonical order. */
  val all: List[GsaoiFilter] =
    List(Z, HeI, PaGamma, JContinuum, J, H, PaBeta, HContinuum, CH4Short, FeII, CH4Long, H20Ice, HeI2p2s, KContinuum1, BrGamma, KContinuum2, KPrime, H2_1_0_S_1, KShort, K, H2_2_1_S_1, CO, Diffuser1, Diffuser2, Blocked)

  /** Select the member of GsaoiFilter with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiFilter] =
    all.find(_.tag === s)

  /** Select the member of GsaoiFilter with the given tag, throwing if absent. */
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