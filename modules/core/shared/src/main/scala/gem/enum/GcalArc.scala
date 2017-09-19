// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for calibration unit arc lamps.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalArc(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GcalArc {

  /** @group Constructors */ case object ArArc extends GcalArc("ArArc", "Ar arc", "Ar arc", false)
  /** @group Constructors */ case object ThArArc extends GcalArc("ThArArc", "ThAr arc", "ThAr arc", false)
  /** @group Constructors */ case object CuArArc extends GcalArc("CuArArc", "CuAr arc", "CuAr arc", false)
  /** @group Constructors */ case object XeArc extends GcalArc("XeArc", "Xe arc", "Xe arc", false)

  /** All members of GcalArc, in canonical order. */
  val all: List[GcalArc] =
    List(ArArc, ThArArc, CuArArc, XeArc)

  /** Select the member of GcalArc with the given tag, if any. */
  def fromTag(s: String): Option[GcalArc] =
    all.find(_.tag === s)

  /** Select the member of GcalArc with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GcalArc =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GcalArcEnumerated: Enumerated[GcalArc] =
    new Enumerated[GcalArc] {
      def all = GcalArc.all
      def tag(a: GcalArc) = a.tag
    }

}