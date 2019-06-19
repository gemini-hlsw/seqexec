// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package enum

import cats.instances.string._
import cats.syntax.eq._
import gem.util.Enumerated

/**
 * Enumerated type for user target type.
 * @group Enumerations (Generated)
 */
sealed abstract class UserTargetType(
  val tag: String,
  val shortName: String,
  val longName: String,
  val obsolete: Boolean
) extends Product with Serializable

object UserTargetType {

  /** @group Constructors */ case object BlindOffset extends UserTargetType("BlindOffset", "Blind Offset", "Blind Offset", false)
  /** @group Constructors */ case object OffAxis extends UserTargetType("OffAxis", "Off Axis", "Off Axis", false)
  /** @group Constructors */ case object TuningStar extends UserTargetType("TuningStar", "Tuning Star", "Tuning Star", false)
  /** @group Constructors */ case object Other extends UserTargetType("Other", "Other", "Other", false)

  /** All members of UserTargetType, in canonical order. */
  val all: List[UserTargetType] =
    List(BlindOffset, OffAxis, TuningStar, Other)

  /** Select the member of UserTargetType with the given tag, if any. */
  def fromTag(s: String): Option[UserTargetType] =
    all.find(_.tag === s)

  /** Select the member of UserTargetType with the given tag, throwing if absent. */
  def unsafeFromTag(s: String): UserTargetType =
    fromTag(s).getOrElse(throw new NoSuchElementException(s"UserTargetType: Invalid tag: '$s'"))

  /** @group Typeclass Instances */
  implicit val UserTargetTypeEnumerated: Enumerated[UserTargetType] =
    new Enumerated[UserTargetType] {
      def all = UserTargetType.all
      def tag(a: UserTargetType) = a.tag
      override def unsafeFromTag(s: String): UserTargetType =
        UserTargetType.unsafeFromTag(s)
    }

}