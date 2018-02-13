// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.math.Angle
import gem.util.Enumerated

/**
 * Enumerated type for GNRIS FPU Slit.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsFpuSlit(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Angle,
  val obsolete: Boolean
) extends Product with Serializable {
  type Self = this.type
}

object GnirsFpuSlit {

  type Aux[A] = GnirsFpuSlit { type Self = A }

  /** @group Constructors */ case object LongSlit1 extends GnirsFpuSlit("LongSlit1", "0.10 arcsec", "0.10 arcsec", Angle.fromDoubleArcseconds(0.100), false)
  /** @group Constructors */ case object LongSlit2 extends GnirsFpuSlit("LongSlit2", "0.15 arcsec", "0.15 arcsec", Angle.fromDoubleArcseconds(0.150), false)
  /** @group Constructors */ case object LongSlit3 extends GnirsFpuSlit("LongSlit3", "0.20 arcsec", "0.20 arcsec", Angle.fromDoubleArcseconds(0.200), false)
  /** @group Constructors */ case object LongSlit4 extends GnirsFpuSlit("LongSlit4", "0.30 arcsec", "0.30 arcsec", Angle.fromDoubleArcseconds(0.300), false)
  /** @group Constructors */ case object LongSlit5 extends GnirsFpuSlit("LongSlit5", "0.45 arcsec", "0.45 arcsec", Angle.fromDoubleArcseconds(0.450), false)
  /** @group Constructors */ case object LongSlit6 extends GnirsFpuSlit("LongSlit6", "0.675 arcsec", "0.675 arcsec", Angle.fromDoubleArcseconds(0.675), false)
  /** @group Constructors */ case object LongSlit7 extends GnirsFpuSlit("LongSlit7", "1.0 arcsec", "1.0 arcsec", Angle.fromDoubleArcseconds(1.000), false)
  /** @group Constructors */ case object LongSlit8 extends GnirsFpuSlit("LongSlit8", "3.0 arcsec", "3.0 arcsec", Angle.fromDoubleArcseconds(3.000), true)

  /** All members of GnirsFpuSlit, in canonical order. */
  val all: List[GnirsFpuSlit] =
    List(LongSlit1, LongSlit2, LongSlit3, LongSlit4, LongSlit5, LongSlit6, LongSlit7, LongSlit8)

  /** Select the member of GnirsFpuSlit with the given tag, if any. */
  def fromTag(s: String): Option[GnirsFpuSlit] =
    all.find(_.tag === s)

  /** Select the member of GnirsFpuSlit with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GnirsFpuSlit =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GnirsFpuSlitEnumerated: Enumerated[GnirsFpuSlit] =
    new Enumerated[GnirsFpuSlit] {
      def all = GnirsFpuSlit.all
      def tag(a: GnirsFpuSlit) = a.tag
    }

}