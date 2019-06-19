// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Entrance Shutter.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiEntranceShutter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Boolean
) extends Product with Serializable

object GpiEntranceShutter {

  /** @group Constructors */ case object Open extends GpiEntranceShutter("Open", "Open", "Open", true)
  /** @group Constructors */ case object Close extends GpiEntranceShutter("Close", "Close", "Close", false)

  /** All members of GpiEntranceShutter, in canonical order. */
  val all: List[GpiEntranceShutter] =
    List(Open, Close)

  /** Select the member of GpiEntranceShutter with the given tag, if any. */
  def fromTag(s: String): Option[GpiEntranceShutter] =
    all.find(_.tag === s)

  /** Select the member of GpiEntranceShutter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiEntranceShutter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiEntranceShutter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiEntranceShutterEnumerated: Enumerated[GpiEntranceShutter] =
    new Enumerated[GpiEntranceShutter] {
      def all = GpiEntranceShutter.all
      def tag(a: GpiEntranceShutter) = a.tag
      override def unsafeFromTag(s: String): GpiEntranceShutter =
        GpiEntranceShutter.unsafeFromTag(s)
    }

}