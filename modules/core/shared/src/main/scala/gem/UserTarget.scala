// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats._
import cats.implicits._

import gem.enum.UserTargetType

import monocle.macros.Lenses


/** Pairs a `Target` and a `UserTargetType`.
  */
@Lenses final case class UserTarget(target: Target, targetType: UserTargetType)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object UserTarget {

  implicit val OrderUserTarget: Order[UserTarget] =
    Order.by(u => (u.target, u.targetType))

  implicit val OrderingUserTarget: Ordering[UserTarget] =
    OrderUserTarget.toOrdering
}