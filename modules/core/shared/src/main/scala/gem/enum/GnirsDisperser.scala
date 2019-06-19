// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GNIRS Disperser.
 * @group Enumerations (Generated)
 */
sealed abstract class GnirsDisperser(
  val tag: String,
  val shortName: String,
  val longName: String,
  val rulingDensity: Int
) extends Product with Serializable

object GnirsDisperser {

  /** @group Constructors */ case object D10 extends GnirsDisperser("D10", "10", "10 l/mm grating", 10)
  /** @group Constructors */ case object D32 extends GnirsDisperser("D32", "32", "32 l/mm grating", 32)
  /** @group Constructors */ case object D111 extends GnirsDisperser("D111", "111", "111 l/mm grating", 111)

  /** All members of GnirsDisperser, in canonical order. */
  val all: List[GnirsDisperser] =
    List(D10, D32, D111)

  /** Select the member of GnirsDisperser with the given tag, if any. */
  def fromTag(s: String): Option[GnirsDisperser] =
    all.find(_.tag === s)

  /** Select the member of GnirsDisperser with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GnirsDisperser =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GnirsDisperser: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GnirsDisperserEnumerated: Enumerated[GnirsDisperser] =
    new Enumerated[GnirsDisperser] {
      def all = GnirsDisperser.all
      def tag(a: GnirsDisperser) = a.tag
      override def unsafeFromTag(s: String): GnirsDisperser =
        GnirsDisperser.unsafeFromTag(s)
    }

}