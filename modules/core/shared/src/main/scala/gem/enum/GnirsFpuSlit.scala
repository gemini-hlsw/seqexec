// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import gsp.math.Angle

/**
 * Enumerated type for GNIRS FPU Slit.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsFpuSlit(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Angle,
  val obsolete: Boolean
) extends Product with Serializable

object GnirsFpuSlit {

  /** @group Constructors */ case object LongSlit_0_10 extends GnirsFpuSlit("LongSlit_0_10", "0.10 arcsec", "0.10 arcsec", Angle.fromDoubleArcseconds(0.100), false)
  /** @group Constructors */ case object LongSlit_0_15 extends GnirsFpuSlit("LongSlit_0_15", "0.15 arcsec", "0.15 arcsec", Angle.fromDoubleArcseconds(0.150), false)
  /** @group Constructors */ case object LongSlit_0_20 extends GnirsFpuSlit("LongSlit_0_20", "0.20 arcsec", "0.20 arcsec", Angle.fromDoubleArcseconds(0.200), false)
  /** @group Constructors */ case object LongSlit_0_30 extends GnirsFpuSlit("LongSlit_0_30", "0.30 arcsec", "0.30 arcsec", Angle.fromDoubleArcseconds(0.300), false)
  /** @group Constructors */ case object LongSlit_0_45 extends GnirsFpuSlit("LongSlit_0_45", "0.45 arcsec", "0.45 arcsec", Angle.fromDoubleArcseconds(0.450), false)
  /** @group Constructors */ case object LongSlit_0_675 extends GnirsFpuSlit("LongSlit_0_675", "0.675 arcsec", "0.675 arcsec", Angle.fromDoubleArcseconds(0.675), false)
  /** @group Constructors */ case object LongSlit_1_00 extends GnirsFpuSlit("LongSlit_1_00", "1.0 arcsec", "1.0 arcsec", Angle.fromDoubleArcseconds(1.000), false)
  /** @group Constructors */ case object LongSlit_3_00 extends GnirsFpuSlit("LongSlit_3_00", "3.0 arcsec", "3.0 arcsec", Angle.fromDoubleArcseconds(3.000), true)

  /** All members of GnirsFpuSlit, in canonical order. */
  val all: List[GnirsFpuSlit] =
    List(LongSlit_0_10, LongSlit_0_15, LongSlit_0_20, LongSlit_0_30, LongSlit_0_45, LongSlit_0_675, LongSlit_1_00, LongSlit_3_00)

  /** Select the member of GnirsFpuSlit with the given tag, if any. */
  def fromTag(s: String): Option[GnirsFpuSlit] =
    all.find(_.tag === s)

  /** Select the member of GnirsFpuSlit with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsFpuSlit =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GnirsFpuSlit: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GnirsFpuSlitEnumerated: Enumerated[GnirsFpuSlit] =
    new Enumerated[GnirsFpuSlit] {
      def all = GnirsFpuSlit.all
      def tag(a: GnirsFpuSlit) = a.tag
      override def unsafeFromTag(s: String): GnirsFpuSlit =
        GnirsFpuSlit.unsafeFromTag(s)
    }

}