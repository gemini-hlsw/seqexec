// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import scalaz.syntax.equal._
import scalaz.std.string._

/**
 * Enumerated type for Flamingos2 focal plane units.
 * @group Enumerations (Generated)
 */
sealed abstract class F2FpUnit(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Int,
  val decker: String,
  val obsolete: Boolean
)

object F2FpUnit {

  /** @group Constructors */ case object Pinhole extends F2FpUnit("Pinhole", "Pinhole", "2-Pixel Pinhole Grid", 0, "Imaging", false)
  /** @group Constructors */ case object SubPixPinhole extends F2FpUnit("SubPixPinhole", "Sub-Pix Pinhole", "Sub-Pixel Pinhole Gr", 0, "Imaging", false)
  /** @group Constructors */ case object None extends F2FpUnit("None", "None", "Imaging (none)", 0, "Imaging", false)
  /** @group Constructors */ case object Custom extends F2FpUnit("Custom", "Custom", "Custom Mask", 0, "MOS", false)
  /** @group Constructors */ case object LongSlit1 extends F2FpUnit("LongSlit1", "Long Slit 1px", "1-Pixel Long Slit", 1, "Long Slit", false)
  /** @group Constructors */ case object LongSlit2 extends F2FpUnit("LongSlit2", "Long Slit 2px", "2-Pixel Long Slit", 2, "Long Slit", false)
  /** @group Constructors */ case object LongSlit3 extends F2FpUnit("LongSlit3", "Long Slit 3px", "3-Pixel Long Slit", 3, "Long Slit", false)
  /** @group Constructors */ case object LongSlit4 extends F2FpUnit("LongSlit4", "Long Slit 4px", "4-Pixel Long Slit", 4, "Long Slit", false)
  /** @group Constructors */ case object LongSlit6 extends F2FpUnit("LongSlit6", "Long Slit 6px", "6-Pixel Long Slit", 6, "Long Slit", false)
  /** @group Constructors */ case object LongSlit8 extends F2FpUnit("LongSlit8", "Long Slit 8px", "8-Pixel Long Slit", 8, "Long Slit", false)

  /** All members of F2FpUnit, in canonical order. */
  val all: List[F2FpUnit] =
    List(Pinhole, SubPixPinhole, None, Custom, LongSlit1, LongSlit2, LongSlit3, LongSlit4, LongSlit6, LongSlit8)

  /** Select the member of F2FpUnit with the given tag, if any. */
  def fromTag(s: String): Option[F2FpUnit] =
    all.find(_.tag === s)

  /** Select the member of F2FpUnit with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): F2FpUnit =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val F2FpUnitEnumerated: Enumerated[F2FpUnit] =
    new Enumerated[F2FpUnit] {
      def all = F2FpUnit.all
      def tag(a: F2FpUnit) = a.tag
    }

}