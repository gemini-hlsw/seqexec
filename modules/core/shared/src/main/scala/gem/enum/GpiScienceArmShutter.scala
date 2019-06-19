// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Science Arm Shutter.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiScienceArmShutter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Boolean
) extends Product with Serializable

object GpiScienceArmShutter {

  /** @group Constructors */ case object Open extends GpiScienceArmShutter("Open", "Open", "Open", true)
  /** @group Constructors */ case object Close extends GpiScienceArmShutter("Close", "Close", "Close", false)

  /** All members of GpiScienceArmShutter, in canonical order. */
  val all: List[GpiScienceArmShutter] =
    List(Open, Close)

  /** Select the member of GpiScienceArmShutter with the given tag, if any. */
  def fromTag(s: String): Option[GpiScienceArmShutter] =
    all.find(_.tag === s)

  /** Select the member of GpiScienceArmShutter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiScienceArmShutter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiScienceArmShutter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiScienceArmShutterEnumerated: Enumerated[GpiScienceArmShutter] =
    new Enumerated[GpiScienceArmShutter] {
      def all = GpiScienceArmShutter.all
      def tag(a: GpiScienceArmShutter) = a.tag
      override def unsafeFromTag(s: String): GpiScienceArmShutter =
        GpiScienceArmShutter.unsafeFromTag(s)
    }

}