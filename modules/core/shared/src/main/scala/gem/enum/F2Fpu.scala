// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for Flamingos2 focal plane units.
 * @group Enumerations (Generated)
 */
sealed abstract class F2Fpu(
  val tag: String,
  val shortName: String,
  val longName: String,
  val slitWidth: Int,
  val decker: String,
  val obsolete: Boolean
) extends Product with Serializable

object F2Fpu {

  /** @group Constructors */ case object Pinhole extends F2Fpu("Pinhole", "Pinhole", "2-Pixel Pinhole Grid", 0, "Imaging", false)
  /** @group Constructors */ case object SubPixPinhole extends F2Fpu("SubPixPinhole", "Sub-Pix Pinhole", "Sub-Pixel Pinhole Gr", 0, "Imaging", false)
  /** @group Constructors */ case object LongSlit1 extends F2Fpu("LongSlit1", "Long Slit 1px", "1-Pixel Long Slit", 1, "Long Slit", false)
  /** @group Constructors */ case object LongSlit2 extends F2Fpu("LongSlit2", "Long Slit 2px", "2-Pixel Long Slit", 2, "Long Slit", false)
  /** @group Constructors */ case object LongSlit3 extends F2Fpu("LongSlit3", "Long Slit 3px", "3-Pixel Long Slit", 3, "Long Slit", false)
  /** @group Constructors */ case object LongSlit4 extends F2Fpu("LongSlit4", "Long Slit 4px", "4-Pixel Long Slit", 4, "Long Slit", false)
  /** @group Constructors */ case object LongSlit6 extends F2Fpu("LongSlit6", "Long Slit 6px", "6-Pixel Long Slit", 6, "Long Slit", false)
  /** @group Constructors */ case object LongSlit8 extends F2Fpu("LongSlit8", "Long Slit 8px", "8-Pixel Long Slit", 8, "Long Slit", false)

  /** All members of F2Fpu, in canonical order. */
  val all: List[F2Fpu] =
    List(Pinhole, SubPixPinhole, LongSlit1, LongSlit2, LongSlit3, LongSlit4, LongSlit6, LongSlit8)

  /** Select the member of F2Fpu with the given tag, if any. */
  def fromTag(s: String): Option[F2Fpu] =
    all.find(_.tag === s)

  /** Select the member of F2Fpu with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): F2Fpu =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val F2FpuEnumerated: Enumerated[F2Fpu] =
    new Enumerated[F2Fpu] {
      def all = F2Fpu.all
      def tag(a: F2Fpu) = a.tag
    }

}