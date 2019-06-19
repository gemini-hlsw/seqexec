// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated
import scala.concurrent.duration._

/**
 * Enumerated type for GSAOI Read Mode.
 * @group Enumerations (Generated)
 */
sealed abstract class GsaoiReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val ndr: Int,
  val readNoise: Int,
  val minimumExposureTime: FiniteDuration,
  val overhead: FiniteDuration
) extends Product with Serializable

object GsaoiReadMode {

  /** @group Constructors */ case object Bright extends GsaoiReadMode("Bright", "Bright", "Bright Objects", 2, 28, 5300.millis, 10000.millis)
  /** @group Constructors */ case object Faint extends GsaoiReadMode("Faint", "Faint", "Faint Objects / Broad-band Imaging", 8, 13, 21500.millis, 26000.millis)
  /** @group Constructors */ case object VeryFaint extends GsaoiReadMode("VeryFaint", "V. Faint", "Very Faint Objects / Narrow-band Imaging", 16, 10, 42500.millis, 48000.millis)

  /** All members of GsaoiReadMode, in canonical order. */
  val all: List[GsaoiReadMode] =
    List(Bright, Faint, VeryFaint)

  /** Select the member of GsaoiReadMode with the given tag, if any. */
  def fromTag(s: String): Option[GsaoiReadMode] =
    all.find(_.tag === s)

  /** Select the member of GsaoiReadMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GsaoiReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GsaoiReadMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GsaoiReadModeEnumerated: Enumerated[GsaoiReadMode] =
    new Enumerated[GsaoiReadMode] {
      def all = GsaoiReadMode.all
      def tag(a: GsaoiReadMode) = a.tag
      override def unsafeFromTag(s: String): GsaoiReadMode =
        GsaoiReadMode.unsafeFromTag(s)
    }

}