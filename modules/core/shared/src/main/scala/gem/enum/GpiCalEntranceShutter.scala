// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Cal Entrance Shutter.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiCalEntranceShutter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Boolean
) extends Product with Serializable

object GpiCalEntranceShutter {

  /** @group Constructors */ case object Open extends GpiCalEntranceShutter("Open", "Open", "Open", true)
  /** @group Constructors */ case object Close extends GpiCalEntranceShutter("Close", "Close", "Close", false)

  /** All members of GpiCalEntranceShutter, in canonical order. */
  val all: List[GpiCalEntranceShutter] =
    List(Open, Close)

  /** Select the member of GpiCalEntranceShutter with the given tag, if any. */
  def fromTag(s: String): Option[GpiCalEntranceShutter] =
    all.find(_.tag === s)

  /** Select the member of GpiCalEntranceShutter with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiCalEntranceShutter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiCalEntranceShutter: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiCalEntranceShutterEnumerated: Enumerated[GpiCalEntranceShutter] =
    new Enumerated[GpiCalEntranceShutter] {
      def all = GpiCalEntranceShutter.all
      def tag(a: GpiCalEntranceShutter) = a.tag
      override def unsafeFromTag(s: String): GpiCalEntranceShutter =
        GpiCalEntranceShutter.unsafeFromTag(s)
    }

}