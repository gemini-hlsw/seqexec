// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq

import gem.enum.{ Site, UserTargetType }
import gem.math.Ephemeris

import monocle.Optional
import monocle.macros.Lenses


/** Pairs a `Target` and a `UserTargetType`.
  */
@Lenses final case class UserTarget(target: Target, targetType: UserTargetType)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object UserTarget {

  val ephemerides: Optional[UserTarget, Map[Site, Ephemeris]] =
    target composeOptional Target.ephemerides

  implicit val EqUserTarget: Eq[UserTarget] =
    Eq.fromUniversalEquals
}