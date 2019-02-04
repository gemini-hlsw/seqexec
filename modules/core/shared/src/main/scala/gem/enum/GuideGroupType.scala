// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for guide group types.
 * @group Enumerations (Generated)
 */
sealed abstract class GuideGroupType(
  val tag: String
) extends Product with Serializable

object GuideGroupType {

  /** @group Constructors */ case object Automatic extends GuideGroupType("Automatic")
  /** @group Constructors */ case object Manual extends GuideGroupType("Manual")

  /** All members of GuideGroupType, in canonical order. */
  val all: List[GuideGroupType] =
    List(Automatic, Manual)

  /** Select the member of GuideGroupType with the given tag, if any. */
  def fromTag(s: String): Option[GuideGroupType] =
    all.find(_.tag === s)

  /** Select the member of GuideGroupType with the given tag, throwing if absent. */
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def unsafeFromTag(s: String): GuideGroupType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s))

  /** @group Typeclass Instances */
  implicit val GuideGroupTypeEnumerated: Enumerated[GuideGroupType] =
    new Enumerated[GuideGroupType] {
      def all = GuideGroupType.all
      def tag(a: GuideGroupType) = a.tag
    }

}