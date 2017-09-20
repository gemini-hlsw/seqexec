// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for calibration unit shutter states.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalShutter(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GcalShutter {

  /** @group Constructors */ case object Open extends GcalShutter("Open", "Open", "Open", false)
  /** @group Constructors */ case object Closed extends GcalShutter("Closed", "Closed", "Closed", false)

  /** All members of GcalShutter, in canonical order. */
  val all: List[GcalShutter] =
    List(Open, Closed)

  /** Select the member of GcalShutter with the given tag, if any. */
  def fromTag(s: String): Option[GcalShutter] =
    all.find(_.tag === s)

  /** Select the member of GcalShutter with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GcalShutter =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GcalShutterEnumerated: Enumerated[GcalShutter] =
    new Enumerated[GcalShutter] {
      def all = GcalShutter.all
      def tag(a: GcalShutter) = a.tag
    }

}