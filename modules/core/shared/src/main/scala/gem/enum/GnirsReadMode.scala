// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GNIRS Read Mode.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsReadMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int,
  val minimumExposureTime: Int,
  val readNoise: Int,
  val readNoiseLow: Int
) extends Product with Serializable {
  type Self = this.type
}

object GnirsReadMode {

  type Aux[A] = GnirsReadMode { type Self = A }

  /** @group Constructors */ case object VeryBright extends GnirsReadMode("VeryBright", "Very bright", "Very Bright Acquisition or High Background", 200, 1, 155, 1)
  /** @group Constructors */ case object Bright extends GnirsReadMode("Bright", "Bright", "Bright objects", 600, 2, 30, 1)
  /** @group Constructors */ case object Faint extends GnirsReadMode("Faint", "Faint", "Faint objects", 3, 9000, 10, 16)
  /** @group Constructors */ case object VeryFaint extends GnirsReadMode("VeryFaint", "Very faint", "Very faint objects", 18000, 4, 7, 32)

  /** All members of GnirsReadMode, in canonical order. */
  val all: List[GnirsReadMode] =
    List(VeryBright, Bright, Faint, VeryFaint)

  /** Select the member of GnirsReadMode with the given tag, if any. */
  def fromTag(s: String): Option[GnirsReadMode] =
    all.find(_.tag === s)

  /** Select the member of GnirsReadMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GnirsReadModeEnumerated: Enumerated[GnirsReadMode] =
    new Enumerated[GnirsReadMode] {
      def all = GnirsReadMode.all
      def tag(a: GnirsReadMode) = a.tag
    }

}