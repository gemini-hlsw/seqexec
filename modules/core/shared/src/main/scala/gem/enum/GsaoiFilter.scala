// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.math.Wavelength
import gem.util.Enumerated
import scala.concurrent.duration.FiniteDuration

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

  /** @group Constructors */ case object Z extends GsaoiFilter("Z", "Z", "Z (1.015 um)", Wavelength.fromPicometers.unsafeGet(1020000), GsaoiReadMode.Faint, new FiniteDuration(26000, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(4619000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.J))
  /** @group Constructors */ case object HeI extends GsaoiFilter("HeI", "HeI", "HeI (1.083 um)", Wavelength.fromPicometers.unsafeGet(1080000), GsaoiReadMode.VeryFaint, new FiniteDuration(72600, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(21792000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.J))
  /** @group Constructors */ case object PaGamma extends GsaoiFilter("PaGamma", "Pagma", "Pa(gamma) (1.094 um)", Wavelength.fromPicometers.unsafeGet(1090000), GsaoiReadMode.VeryFaint, new FiniteDuration(122000, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(36585000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.J))
  /** @group Constructors */ case object JContinuum extends GsaoiFilter("JContinuum", "Jcont", "J-continuum (1.207 um)", Wavelength.fromPicometers.unsafeGet(1210000), GsaoiReadMode.VeryFaint, new FiniteDuration(32600, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(9793000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.J))
  /** @group Constructors */ case object J extends GsaoiFilter("J", "J", "J (1.250 um)", Wavelength.fromPicometers.unsafeGet(1250000), GsaoiReadMode.Faint, new FiniteDuration(5700, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(1004000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.J))
  /** @group Constructors */ case object H extends GsaoiFilter("H", "H", "H (1.635 um)", Wavelength.fromPicometers.unsafeGet(1640000), GsaoiReadMode.Bright, new FiniteDuration(12000, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(460000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.H))
  /** @group Constructors */ case object PaBeta extends GsaoiFilter("PaBeta", "Pabeta", "Pa(beta) (1.282 um)", Wavelength.fromPicometers.unsafeGet(1280000), GsaoiReadMode.Faint, new FiniteDuration(21800, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(3879000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.J))
  /** @group Constructors */ case object HContinuum extends GsaoiFilter("HContinuum", "Hcont", "H-continuum (1.570 um)", Wavelength.fromPicometers.unsafeGet(1570000), GsaoiReadMode.Faint, new FiniteDuration(31200, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(5545000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.H))
  /** @group Constructors */ case object CH4Short extends GsaoiFilter("CH4Short", "CH4short", "CH4(short) (1.580 um)", Wavelength.fromPicometers.unsafeGet(1580000), GsaoiReadMode.Faint, new FiniteDuration(6600, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(1174000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.H))
  /** @group Constructors */ case object FeII extends GsaoiFilter("FeII", "FeII1644", "[Fe II] (1.644 um)", Wavelength.fromPicometers.unsafeGet(1640000), GsaoiReadMode.Faint, new FiniteDuration(24900, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(4416000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.H))
  /** @group Constructors */ case object CH4Long extends GsaoiFilter("CH4Long", "CH4long", "CH4(long) (1.690 um)", Wavelength.fromPicometers.unsafeGet(1690000), GsaoiReadMode.Faint, new FiniteDuration(6800, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(1202000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.H))
  /** @group Constructors */ case object H20Ice extends GsaoiFilter("H20Ice", "H20ice", "H20 ice (2.000 um)", Wavelength.fromPicometers.unsafeGet(2000000), GsaoiReadMode.Faint, new FiniteDuration(19100, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(3395000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object HeI2p2s extends GsaoiFilter("HeI2p2s", "HeI2p2s", "HeI (2p2s) (2.058 um)", Wavelength.fromPicometers.unsafeGet(2060000), GsaoiReadMode.Faint, new FiniteDuration(28300, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(5032000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object KContinuum1 extends GsaoiFilter("KContinuum1", "Kcontshrt", "Ks-continuum (2.093 um)", Wavelength.fromPicometers.unsafeGet(2090000), GsaoiReadMode.Faint, new FiniteDuration(7800, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(6069000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object BrGamma extends GsaoiFilter("BrGamma", "Brgma", "Br(gamma) (2.166 um)", Wavelength.fromPicometers.unsafeGet(2170000), GsaoiReadMode.Faint, new FiniteDuration(31000, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(5496000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object KContinuum2 extends GsaoiFilter("KContinuum2", "Kcontlong", "Kl-continuum (2.270 um)", Wavelength.fromPicometers.unsafeGet(2270000), GsaoiReadMode.Faint, new FiniteDuration(33300, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(5911000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object KPrime extends GsaoiFilter("KPrime", "Kprime", "K(prime) (2.120 um)", Wavelength.fromPicometers.unsafeGet(2120000), GsaoiReadMode.Bright, new FiniteDuration(14800, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(566000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object H2_1_0_S_1 extends GsaoiFilter("H2_1_0_S_1", "H2(1-0)", "H2 1-0 S(1) (2.122 um)", Wavelength.fromPicometers.unsafeGet(2120000), GsaoiReadMode.Faint, new FiniteDuration(27500, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(5400000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object KShort extends GsaoiFilter("KShort", "Kshort", "K(short) (2.150 um)", Wavelength.fromPicometers.unsafeGet(2150000), GsaoiReadMode.Bright, new FiniteDuration(14400, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(551000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object K extends GsaoiFilter("K", "K", "K (2.200 um)", Wavelength.fromPicometers.unsafeGet(2200000), GsaoiReadMode.Bright, new FiniteDuration(12300, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(470000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object H2_2_1_S_1 extends GsaoiFilter("H2_2_1_S_1", "H2(2-1)", "H2 2-1 S(1) (2.248 um)", Wavelength.fromPicometers.unsafeGet(2250000), GsaoiReadMode.Faint, new FiniteDuration(32600, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(5784000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object CO extends GsaoiFilter("CO", "CO2360", "CO (2.360 um)", Wavelength.fromPicometers.unsafeGet(2360000), GsaoiReadMode.Faint, new FiniteDuration(7700, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(1370000, java.util.concurrent.TimeUnit.MILLISECONDS), Some(MagnitudeBand.K))
  /** @group Constructors */ case object Diffuser1 extends GsaoiFilter("Diffuser1", "Diffuser1", "Diffuser1", Wavelength.fromPicometers.unsafeGet(0), GsaoiReadMode.Bright, new FiniteDuration(0, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(0, java.util.concurrent.TimeUnit.MILLISECONDS), Option.empty[MagnitudeBand])
  /** @group Constructors */ case object Diffuser2 extends GsaoiFilter("Diffuser2", "Diffuser2", "Diffuser2", Wavelength.fromPicometers.unsafeGet(0), GsaoiReadMode.Bright, new FiniteDuration(0, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(0, java.util.concurrent.TimeUnit.MILLISECONDS), Option.empty[MagnitudeBand])
  /** @group Constructors */ case object Blocked extends GsaoiFilter("Blocked", "Blocked", "Blocked", Wavelength.fromPicometers.unsafeGet(0), GsaoiReadMode.Bright, new FiniteDuration(0, java.util.concurrent.TimeUnit.MILLISECONDS), new FiniteDuration(0, java.util.concurrent.TimeUnit.MILLISECONDS), Option.empty[MagnitudeBand])

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