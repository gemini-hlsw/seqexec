// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for Gsaoi Utility Wheel.
 * @group Enumerations (Generated)
 */
sealed abstract class GsaoiUtilityWheel(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GsaoiUtilityWheel {

  /** @group Constructors */ case object ExtrafocalLens1 extends GsaoiUtilityWheel("ExtrafocalLens1", "xf 1", "Extra-focal lens 1")
  /** @group Constructors */ case object ExtrafocalLens2 extends GsaoiUtilityWheel("ExtrafocalLens2", "xf 2", "Extra-focal lens 2")
  /** @group Constructors */ case object PupilImager extends GsaoiUtilityWheel("PupilImager", "pupil", "Pupil Imager")
  /** @group Constructors */ case object Clear extends GsaoiUtilityWheel("Clear", "clear", "Clear")

  /** All members of GsaoiUtilityWheel, in canonical order. */
  val all: List[GsaoiUtilityWheel] =
    List(ExtrafocalLens1, ExtrafocalLens2, PupilImager, Clear)

  /** Select the member of GsaoiUtilityWheel with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiUtilityWheel] =
    all.find(_.tag === s)

  /** Select the member of GsaoiUtilityWheel with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GsaoiUtilityWheel =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GsaoiUtilityWheel: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GsaoiUtilityWheelEnumerated: Enumerated[GsaoiUtilityWheel] =
    new Enumerated[GsaoiUtilityWheel] {
      def all = GsaoiUtilityWheel.all
      def tag(a: GsaoiUtilityWheel) = a.tag
      override def unsafeFromTag(s: String): GsaoiUtilityWheel =
        GsaoiUtilityWheel.unsafeFromTag(s)
    }

}