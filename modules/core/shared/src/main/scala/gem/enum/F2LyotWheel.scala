// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for Flamingos2 Lyot wheel.
 * @group Enumerations (Generated)
 */
sealed abstract class F2LyotWheel(
  val tag: String,
  val shortName: String,
  val longName: String,
  val plateScale: Double,
  val pixelScale: Double,
  val obsolete: Boolean
) extends Product with Serializable

object F2LyotWheel {

  /** @group Constructors */ case object F16 extends F2LyotWheel("F16", "f/16", "f/16 (Open)", 1.61, 0.18, false)
  /** @group Constructors */ case object F32High extends F2LyotWheel("F32High", "f/32 High", "f/32 MCAO high background", 0.805, 0.09, true)
  /** @group Constructors */ case object F32Low extends F2LyotWheel("F32Low", "f/32 Low", "f/32 MCAO low background", 0.805, 0.09, true)
  /** @group Constructors */ case object F33Gems extends F2LyotWheel("F33Gems", "f/33 GeMS", "f/33 (GeMS)", 0.784, 0.09, true)
  /** @group Constructors */ case object GemsUnder extends F2LyotWheel("GemsUnder", "GeMS Under", "f/33 (GeMS under-sized)", 0.784, 0.09, false)
  /** @group Constructors */ case object GemsOver extends F2LyotWheel("GemsOver", "GeMS Over", "f/33 (GeMS over-sized)", 0.784, 0.09, false)
  /** @group Constructors */ case object HartmannA extends F2LyotWheel("HartmannA", "Hartmann A (H1)", "Hartmann A (H1)", 0.0, 0.0, false)
  /** @group Constructors */ case object HartmannB extends F2LyotWheel("HartmannB", "Hartmann B (H2)", "Hartmann B (H2)", 0.0, 0.0, false)

  /** All members of F2LyotWheel, in canonical order. */
  val all: List[F2LyotWheel] =
    List(F16, F32High, F32Low, F33Gems, GemsUnder, GemsOver, HartmannA, HartmannB)

  /** Select the member of F2LyotWheel with the given tag, if any. */
  def fromTag(s: String): Option[F2LyotWheel] =
    all.find(_.tag === s)

  /** Select the member of F2LyotWheel with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): F2LyotWheel =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val F2LyotWheelEnumerated: Enumerated[F2LyotWheel] =
    new Enumerated[F2LyotWheel] {
      def all = F2LyotWheel.all
      def tag(a: F2LyotWheel) = a.tag
    }

}