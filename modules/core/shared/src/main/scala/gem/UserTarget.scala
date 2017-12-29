// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum.UserTargetType

/** Pairs a `Target` and a `UserTargetType`.
  */
final case class UserTarget(target: Target, targetType: UserTargetType)