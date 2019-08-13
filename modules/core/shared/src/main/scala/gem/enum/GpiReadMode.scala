// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for GPI ReadMode.
 * @group Enumerations (Generated)
 */
sealed abstract class GpiReadMode(
  val tag: String,
  val longName: String,
  val value: Int
) extends Product with Serializable

object GpiReadMode {

  /** @group Constructors */ case object Single extends GpiReadMode("Single", "Fast", 1)
  /** @group Constructors */ case object CDS extends GpiReadMode("CDS", "Single CDS", 2)
  /** @group Constructors */ case object MCDS extends GpiReadMode("MCDS", "Multiple CDS", 3)
  /** @group Constructors */ case object UTR extends GpiReadMode("UTR", "UTR", 4)

  /** All members of GpiReadMode, in canonical order. */
  val all: List[GpiReadMode] =
    List(Single, CDS, MCDS, UTR)

  /** Select the member of GpiReadMode with the given tag, if any. */
  def fromTag(s: String): Option[GpiReadMode] =
    all.find(_.tag === s)

  /** Select the member of GpiReadMode with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): GpiReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"GpiReadMode: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val GpiReadModeEnumerated: Enumerated[GpiReadMode] =
    new Enumerated[GpiReadMode] {
      def all = GpiReadMode.all
      def tag(a: GpiReadMode) = a.tag
      override def unsafeFromTag(s: String): GpiReadMode =
        GpiReadMode.unsafeFromTag(s)
    }

}