// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS Electric Offsetting.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosEOffsetting(
  val tag: String,
  val description: String,
  val toBoolean: Boolean
) extends Product with Serializable

object GmosEOffsetting {

  /** @group Constructors */ case object On extends GmosEOffsetting("On", "Electronic Offsetting On", true)
  /** @group Constructors */ case object Off extends GmosEOffsetting("Off", "Electronic Offsetting Off", false)

  /** All members of GmosEOffsetting, in canonical order. */
  val all: List[GmosEOffsetting] =
    List(On, Off)

  /** Select the member of GmosEOffsetting with the given tag, if any. */
  def fromTag(s: String): Option[GmosEOffsetting] =
    all.find(_.tag === s)

  /** Select the member of GmosEOffsetting with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosEOffsetting =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosEOffsettingEnumerated: Enumerated[GmosEOffsetting] =
    new Enumerated[GmosEOffsetting] {
      def all = GmosEOffsetting.all
      def tag(a: GmosEOffsetting) = a.tag
    }

}