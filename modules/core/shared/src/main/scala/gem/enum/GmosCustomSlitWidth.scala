// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS custom slit width.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosCustomSlitWidth(
  val tag: String,
  val shortName: String,
  val longName: String,
  val width: gem.math.Angle
) extends Product with Serializable

object GmosCustomSlitWidth {

  /** @group Constructors */ case object CustomWidth_0_25 extends GmosCustomSlitWidth("CustomWidth_0_25", "0.25", "0.25 arcsec", gem.math.Angle.fromDoubleArcseconds(0.25))
  /** @group Constructors */ case object CustomWidth_0_50 extends GmosCustomSlitWidth("CustomWidth_0_50", "0.50", "0.50 arcsec", gem.math.Angle.fromDoubleArcseconds(0.5))
  /** @group Constructors */ case object CustomWidth_0_75 extends GmosCustomSlitWidth("CustomWidth_0_75", "0.75", "0.75 arcsec", gem.math.Angle.fromDoubleArcseconds(0.75))
  /** @group Constructors */ case object CustomWidth_1_00 extends GmosCustomSlitWidth("CustomWidth_1_00", "1.00", "1.00 arcsec", gem.math.Angle.fromDoubleArcseconds(1.0))
  /** @group Constructors */ case object CustomWidth_1_50 extends GmosCustomSlitWidth("CustomWidth_1_50", "1.50", "1.50 arcsec", gem.math.Angle.fromDoubleArcseconds(1.5))
  /** @group Constructors */ case object CustomWidth_2_00 extends GmosCustomSlitWidth("CustomWidth_2_00", "2.00", "2.00 arcsec", gem.math.Angle.fromDoubleArcseconds(2.0))
  /** @group Constructors */ case object CustomWidth_5_00 extends GmosCustomSlitWidth("CustomWidth_5_00", "5.00", "5.00 arcsec", gem.math.Angle.fromDoubleArcseconds(5.0))

  /** All members of GmosCustomSlitWidth, in canonical order. */
  val all: List[GmosCustomSlitWidth] =
    List(CustomWidth_0_25, CustomWidth_0_50, CustomWidth_0_75, CustomWidth_1_00, CustomWidth_1_50, CustomWidth_2_00, CustomWidth_5_00)

  /** Select the member of GmosCustomSlitWidth with the given tag, if any. */
  def fromTag(s: String): Option[GmosCustomSlitWidth] =
    all.find(_.tag === s)

  /** Select the member of GmosCustomSlitWidth with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosCustomSlitWidth =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosCustomSlitWidthEnumerated: Enumerated[GmosCustomSlitWidth] =
    new Enumerated[GmosCustomSlitWidth] {
      def all = GmosCustomSlitWidth.all
      def tag(a: GmosCustomSlitWidth) = a.tag
    }

}