// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS amp count.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosAmpCount(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosAmpCount {

  /** @group Constructors */ case object Three extends GmosAmpCount("Three", "Three", "Three")
  /** @group Constructors */ case object Six extends GmosAmpCount("Six", "Six", "Six")
  /** @group Constructors */ case object Twelve extends GmosAmpCount("Twelve", "Twelve", "Twelve")

  /** All members of GmosAmpCount, in canonical order. */
  val all: List[GmosAmpCount] =
    List(Three, Six, Twelve)

  /** Select the member of GmosAmpCount with the given tag, if any. */
  def fromTag(s: String): Option[GmosAmpCount] =
    all.find(_.tag === s)

  /** Select the member of GmosAmpCount with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosAmpCount =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosAmpCountEnumerated: Enumerated[GmosAmpCount] =
    new Enumerated[GmosAmpCount] {
      def all = GmosAmpCount.all
      def tag(a: GmosAmpCount) = a.tag
    }

}