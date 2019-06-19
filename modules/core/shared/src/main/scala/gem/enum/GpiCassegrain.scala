// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI Cassegrain.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiCassegrain(
  val tag: String,
  val shortName: String,
  val longName: String,
  val value: Int
) extends Product with Serializable

object GpiCassegrain {

  /** @group Constructors */ case object A0 extends GpiCassegrain("A0", "0", "0", 0)
  /** @group Constructors */ case object A180 extends GpiCassegrain("A180", "180", "180", 180)

  /** All members of GpiCassegrain, in canonical order. */
  val all: List[GpiCassegrain] =
    List(A0, A180)

  /** Select the member of GpiCassegrain with the given tag, if any. */
  def fromTag(s: String): Option[GpiCassegrain] =
    all.find(_.tag === s)

  /** Select the member of GpiCassegrain with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiCassegrain =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiCassegrain: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiCassegrainEnumerated: Enumerated[GpiCassegrain] =
    new Enumerated[GpiCassegrain] {
      def all = GpiCassegrain.all
      def tag(a: GpiCassegrain) = a.tag
      override def unsafeFromTag(s: String): GpiCassegrain =
        GpiCassegrain.unsafeFromTag(s)
    }

}