// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS X-binning.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosXBinning(
  val tag: String,
  val shortName: String,
  val longName: String,
  val count: Int
) extends Product with Serializable

object GmosXBinning {

  /** @group Constructors */ case object One extends GmosXBinning("One", "1", "One", 1)
  /** @group Constructors */ case object Two extends GmosXBinning("Two", "2", "Two", 2)
  /** @group Constructors */ case object Four extends GmosXBinning("Four", "4", "Four", 4)

  /** All members of GmosXBinning, in canonical order. */
  val all: List[GmosXBinning] =
    List(One, Two, Four)

  /** Select the member of GmosXBinning with the given tag, if any. */
  def fromTag(s: String): Option[GmosXBinning] =
    all.find(_.tag === s)

  /** Select the member of GmosXBinning with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosXBinning =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosXBinningEnumerated: Enumerated[GmosXBinning] =
    new Enumerated[GmosXBinning] {
      def all = GmosXBinning.all
      def tag(a: GmosXBinning) = a.tag
    }

}