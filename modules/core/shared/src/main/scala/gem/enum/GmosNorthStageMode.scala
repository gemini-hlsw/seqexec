// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS North stage modes.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosNorthStageMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GmosNorthStageMode {

  /** @group Constructors */ case object NoFollow extends GmosNorthStageMode("NoFollow", "No Follow", "Do Not Follow", false)
  /** @group Constructors */ case object FollowXyz extends GmosNorthStageMode("FollowXyz", "Follow XYZ", "Follow in XYZ(focus)", true)
  /** @group Constructors */ case object FollowXy extends GmosNorthStageMode("FollowXy", "Follow XY", "Follow in XY", false)
  /** @group Constructors */ case object FollowZ extends GmosNorthStageMode("FollowZ", "Follow Z", "Follow in Z Only", true)

  /** All members of GmosNorthStageMode, in canonical order. */
  val all: List[GmosNorthStageMode] =
    List(NoFollow, FollowXyz, FollowXy, FollowZ)

  /** Select the member of GmosNorthStageMode with the given tag, if any. */
  def fromTag(s: String): Option[GmosNorthStageMode] =
    all.find(_.tag === s)

  /** Select the member of GmosNorthStageMode with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosNorthStageMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosNorthStageModeEnumerated: Enumerated[GmosNorthStageMode] =
    new Enumerated[GmosNorthStageMode] {
      def all = GmosNorthStageMode.all
      def tag(a: GmosNorthStageMode) = a.tag
    }

}