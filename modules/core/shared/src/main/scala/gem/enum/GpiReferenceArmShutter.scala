// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Reference Arm Shutter.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiReferenceArmShutter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Boolean
) extends Product with Serializable

object GpiReferenceArmShutter {

  /** @group Constructors */ case object Open extends GpiReferenceArmShutter("Open", "Open", "Open", true)
  /** @group Constructors */ case object Close extends GpiReferenceArmShutter("Close", "Close", "Close", false)

  /** All members of GpiReferenceArmShutter, in canonical order. */
  val all: List[GpiReferenceArmShutter] =
    List(Open, Close)

  /** Select the member of GpiReferenceArmShutter with the given tag, if any. */
  def fromTag(s: String): Option[GpiReferenceArmShutter] =
    all.find(_.tag === s)

  /** Select the member of GpiReferenceArmShutter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiReferenceArmShutter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiReferenceArmShutter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiReferenceArmShutterEnumerated: Enumerated[GpiReferenceArmShutter] =
    new Enumerated[GpiReferenceArmShutter] {
      def all = GpiReferenceArmShutter.all
      def tag(a: GpiReferenceArmShutter) = a.tag
      override def unsafeFromTag(s: String): GpiReferenceArmShutter =
        GpiReferenceArmShutter.unsafeFromTag(s)
    }

}