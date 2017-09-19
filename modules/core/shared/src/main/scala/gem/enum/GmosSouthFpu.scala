// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS South focal plane units.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthFpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Option[gem.math.Angle]
) extends Product with Serializable

object GmosSouthFpu {

  /** @group Constructors */ case object Longslit1 extends GmosSouthFpu("Longslit1", "0.25arcsec", "Longslit 0.25 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(0.25)))
  /** @group Constructors */ case object Longslit2 extends GmosSouthFpu("Longslit2", "0.50arcsec", "Longslit 0.50 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(0.5)))
  /** @group Constructors */ case object Longslit3 extends GmosSouthFpu("Longslit3", "0.75arcsec", "Longslit 0.75 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(0.75)))
  /** @group Constructors */ case object Longslit4 extends GmosSouthFpu("Longslit4", "1.0arcsec", "Longslit 1.00 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(1.0)))
  /** @group Constructors */ case object Longslit5 extends GmosSouthFpu("Longslit5", "1.5arcsec", "Longslit 1.50 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(1.5)))
  /** @group Constructors */ case object Longslit6 extends GmosSouthFpu("Longslit6", "2.0arcsec", "Longslit 2.00 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(2.0)))
  /** @group Constructors */ case object Longslit7 extends GmosSouthFpu("Longslit7", "5.0arcsec", "Longslit 5.00 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(5.0)))
  /** @group Constructors */ case object Ifu1 extends GmosSouthFpu("Ifu1", "IFU-2", "IFU 2 Slits", Option.empty[gem.math.Angle])
  /** @group Constructors */ case object Ifu2 extends GmosSouthFpu("Ifu2", "IFU-B", "IFU Left Slit (blue)", Option.empty[gem.math.Angle])
  /** @group Constructors */ case object Ifu3 extends GmosSouthFpu("Ifu3", "IFU-R", "IFU Right Slit (red)", Option.empty[gem.math.Angle])
  /** @group Constructors */ case object Bhros extends GmosSouthFpu("Bhros", "bHROS", "bHROS", Option.empty[gem.math.Angle])
  /** @group Constructors */ case object IfuN extends GmosSouthFpu("IfuN", "IFU-NS-2", "IFU N and S 2 Slits", Option.empty[gem.math.Angle])
  /** @group Constructors */ case object IfuNB extends GmosSouthFpu("IfuNB", "IFU-NS-B", "IFU N and S Left Slit (blue)", Option.empty[gem.math.Angle])
  /** @group Constructors */ case object IfuNR extends GmosSouthFpu("IfuNR", "IFU-NS-R", "IFU N and S Right Slit (red)", Option.empty[gem.math.Angle])
  /** @group Constructors */ case object Ns1 extends GmosSouthFpu("Ns1", "NS0.5arcsec", "N and S 0.50 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(0.5)))
  /** @group Constructors */ case object Ns2 extends GmosSouthFpu("Ns2", "NS0.75arcsec", "N and S 0.75 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(0.75)))
  /** @group Constructors */ case object Ns3 extends GmosSouthFpu("Ns3", "NS1.0arcsec", "N and S 1.00 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(1.0)))
  /** @group Constructors */ case object Ns4 extends GmosSouthFpu("Ns4", "NS1.5arcsec", "N and S 1.50 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(1.5)))
  /** @group Constructors */ case object Ns5 extends GmosSouthFpu("Ns5", "NS2.0arcsec", "N and S 2.00 arcsec", Some(gem.math.Angle.fromDoubleArcseconds(2.0)))

  /** All members of GmosSouthFpu, in canonical order. */
  val all: List[GmosSouthFpu] =
    List(Longslit1, Longslit2, Longslit3, Longslit4, Longslit5, Longslit6, Longslit7, Ifu1, Ifu2, Ifu3, Bhros, IfuN, IfuNB, IfuNR, Ns1, Ns2, Ns3, Ns4, Ns5)

  /** Select the member of GmosSouthFpu with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthFpu] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthFpu with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosSouthFpu =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosSouthFpuEnumerated: Enumerated[GmosSouthFpu] =
    new Enumerated[GmosSouthFpu] {
      def all = GmosSouthFpu.all
      def tag(a: GmosSouthFpu) = a.tag
    }

}