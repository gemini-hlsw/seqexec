// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Artificial Source Unit.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiASU(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Boolean
) extends Product with Serializable

object GpiASU {

  /** @group Constructors */ case object On extends GpiASU("On", "On", "On", true)
  /** @group Constructors */ case object Off extends GpiASU("Off", "Off", "Off", false)

  /** All members of GpiASU, in canonical order. */
  val all: List[GpiASU] =
    List(On, Off)

  /** Select the member of GpiASU with the given tag, if any. */
  def fromTag(s: String): Option[GpiASU] =
    all.find(_.tag === s)

  /** Select the member of GpiASU with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiASU =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiASU: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiASUEnumerated: Enumerated[GpiASU] =
    new Enumerated[GpiASU] {
      def all = GpiASU.all
      def tag(a: GpiASU) = a.tag
      override def unsafeFromTag(s: String): GpiASU =
        GpiASU.unsafeFromTag(s)
    }

}