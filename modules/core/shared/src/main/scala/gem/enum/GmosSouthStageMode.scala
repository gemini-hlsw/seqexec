// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.syntax.eq._
import cats.instances.string._
import gem.util.Enumerated

/**
 * Enumerated type for GMOS South stage mode.
 * @group Enumerations (Generated)
 */
sealed abstract class GmosSouthStageMode(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object GmosSouthStageMode {

  /** @group Constructors */ case object NoFollow extends GmosSouthStageMode("NoFollow", "No Follow", "Do Not Follow", false)
  /** @group Constructors */ case object FollowXyz extends GmosSouthStageMode("FollowXyz", "Follow XYZ", "Follow in XYZ(focus)", false)
  /** @group Constructors */ case object FollowXy extends GmosSouthStageMode("FollowXy", "Follow XY", "Follow in XY", true)
  /** @group Constructors */ case object FollowZ extends GmosSouthStageMode("FollowZ", "Follow Z", "Follow in Z Only", false)

  /** All members of GmosSouthStageMode, in canonical order. */
  val all: List[GmosSouthStageMode] =
    List(NoFollow, FollowXyz, FollowXy, FollowZ)

  /** Select the member of GmosSouthStageMode with the given tag, if any. */
  def fromTag(s: String): Option[GmosSouthStageMode] =
    all.find(_.tag === s)

  /** Select the member of GmosSouthStageMode with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GmosSouthStageMode =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GmosSouthStageModeEnumerated: Enumerated[GmosSouthStageMode] =
    new Enumerated[GmosSouthStageMode] {
      def all = GmosSouthStageMode.all
      def tag(a: GmosSouthStageMode) = a.tag
    }

}