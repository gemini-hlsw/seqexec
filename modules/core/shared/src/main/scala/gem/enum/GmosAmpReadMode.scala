// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS amp read mode.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosAmpReadMode(
  val tag: String,
  val shortName: String,
  val longName: String
) extends Product with Serializable

object GmosAmpReadMode {

  /** @group Constructors */ case object Slow extends GmosAmpReadMode("Slow", "slow", "Slow")
  /** @group Constructors */ case object Fast extends GmosAmpReadMode("Fast", "fast", "Fast")

  /** All members of GmosAmpReadMode, in canonical order. */
  val all: List[GmosAmpReadMode] =
    List(Slow, Fast)

  /** Select the member of GmosAmpReadMode with the given tag, if any. */
  def fromTag(s: String): Option[GmosAmpReadMode] =
    all.find(_.tag === s)

  /** Select the member of GmosAmpReadMode with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosAmpReadMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosAmpReadModeEnumerated: Enumerated[GmosAmpReadMode] =
    new Enumerated[GmosAmpReadMode] {
      def all = GmosAmpReadMode.all
      def tag(a: GmosAmpReadMode) = a.tag
    }

}