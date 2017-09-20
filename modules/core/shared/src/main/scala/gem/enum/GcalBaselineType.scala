// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for calibration baseline type.
 * @group Enumerations (Generated)
 */
sealed abstract class GcalBaselineType(
  val tag: String
) extends Product with Serializable

object GcalBaselineType {

  /** @group Constructors */ case object Day extends GcalBaselineType("Day")
  /** @group Constructors */ case object Night extends GcalBaselineType("Night")

  /** All members of GcalBaselineType, in canonical order. */
  val all: List[GcalBaselineType] =
    List(Day, Night)

  /** Select the member of GcalBaselineType with the given tag, if any. */
  def fromTag(s: String): Option[GcalBaselineType] =
    all.find(_.tag === s)

  /** Select the member of GcalBaselineType with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GcalBaselineType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GcalBaselineTypeEnumerated: Enumerated[GcalBaselineType] =
    new Enumerated[GcalBaselineType] {
      def all = GcalBaselineType.all
      def tag(a: GcalBaselineType) = a.tag
    }

}