// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for Gsaoi Region of Interest.
 * @group Enumerations (Generated)
 */
sealed abstract class GsaoiRoi(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GsaoiRoi {

  /** @group Constructors */ case object ExtrafocalLens1 extends GsaoiRoi("ExtrafocalLens1", "xf 1", "Extra-focal lens 1")
  /** @group Constructors */ case object ExtrafocalLens2 extends GsaoiRoi("ExtrafocalLens2", "xf 2", "Extra-focal lens 2")
  /** @group Constructors */ case object PupilImager extends GsaoiRoi("PupilImager", "pupil", "Pupil Imager")
  /** @group Constructors */ case object Clear extends GsaoiRoi("Clear", "clear", "Clear")

  /** All members of GsaoiRoi, in canonical order. */
  val all: List[GsaoiRoi] =
    List(ExtrafocalLens1, ExtrafocalLens2, PupilImager, Clear)

  /** Select the member of GsaoiRoi with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiRoi] =
    all.find(_.tag === s)

  /** Select the member of GsaoiRoi with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GsaoiRoi =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GsaoiRoi: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GsaoiRoiEnumerated: Enumerated[GsaoiRoi] =
    new Enumerated[GsaoiRoi] {
      def all = GsaoiRoi.all
      def tag(a: GsaoiRoi) = a.tag
      override def unsafeFromTag(s: String): GsaoiRoi =
        GsaoiRoi.unsafeFromTag(s)
    }

}