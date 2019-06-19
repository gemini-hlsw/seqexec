// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats._
import cats.implicits._

import gem.enum.UserTargetType

import monocle.macros.Lenses


/** Pairs a `Target` and a `UserTargetType`.
  */
@Lenses final case class UserTarget(target: Target, targetType: UserTargetType)

object UserTarget {

  /** UserTarget identifier. */
  final case class Id(toInt: Int) extends AnyVal

  object Id {
    /** Ids ordered by wrapped integer value. */
    implicit val IdOrder: Order[Id] =
      Order.by(_.toInt)
  }

  implicit val OrderUserTarget: Order[UserTarget] =
    Order.by(u => (u.target, u.targetType))

  implicit val OrderingUserTarget: Ordering[UserTarget] =
    OrderUserTarget.toOrdering
}