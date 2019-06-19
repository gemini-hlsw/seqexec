// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for asterism types.
 * @group Enumerations (Generated)
 */
sealed abstract class AsterismType(
  val tag: String
) extends Product with Serializable

object AsterismType {

  /** @group Constructors */ case object GhostDualTarget extends AsterismType("GhostDualTarget")
  /** @group Constructors */ case object SingleTarget extends AsterismType("SingleTarget")

  /** All members of AsterismType, in canonical order. */
  val all: List[AsterismType] =
    List(GhostDualTarget, SingleTarget)

  /** Select the member of AsterismType with the given tag, if any. */
  def fromTag(s: String): Option[AsterismType] =
    all.find(_.tag === s)

  /** Select the member of AsterismType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): AsterismType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"AsterismType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val AsterismTypeEnumerated: Enumerated[AsterismType] =
    new Enumerated[AsterismType] {
      def all = AsterismType.all
      def tag(a: AsterismType) = a.tag
      override def unsafeFromTag(s: String): AsterismType =
        AsterismType.unsafeFromTag(s)
    }

}