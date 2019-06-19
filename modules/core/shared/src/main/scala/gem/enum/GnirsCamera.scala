// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GNIRS Camera.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsCamera(
  val tag: String,
  val shortName: String,
  val longName: String,
  val pixelScale: GnirsPixelScale
) extends Product with Serializable

object GnirsCamera {

  /** @group Constructors */ case object LongBlue extends GnirsCamera("LongBlue", "Long blue", "Long blue camera", GnirsPixelScale.PixelScale_0_05)
  /** @group Constructors */ case object LongRed extends GnirsCamera("LongRed", "Long red", "Long red camera", GnirsPixelScale.PixelScale_0_05)
  /** @group Constructors */ case object ShortBlue extends GnirsCamera("ShortBlue", "Short blue", "Short blue camera", GnirsPixelScale.PixelScale_0_15)
  /** @group Constructors */ case object ShortRed extends GnirsCamera("ShortRed", "Short red", "Short red camera", GnirsPixelScale.PixelScale_0_15)

  /** All members of GnirsCamera, in canonical order. */
  val all: List[GnirsCamera] =
    List(LongBlue, LongRed, ShortBlue, ShortRed)

  /** Select the member of GnirsCamera with the given tag, if any. */
  def fromTag(s: String): Option[GnirsCamera] =
    all.find(_.tag === s)

  /** Select the member of GnirsCamera with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsCamera =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GnirsCamera: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GnirsCameraEnumerated: Enumerated[GnirsCamera] =
    new Enumerated[GnirsCamera] {
      def all = GnirsCamera.all
      def tag(a: GnirsCamera) = a.tag
      override def unsafeFromTag(s: String): GnirsCamera =
        GnirsCamera.unsafeFromTag(s)
    }

}