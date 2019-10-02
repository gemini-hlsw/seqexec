// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

sealed trait NSAction extends Product with Serializable

object NSAction {
  case object Start extends NSAction
  case object NodStart extends NSAction
  case object NodComplete extends NSAction
  case object StageObserveStart extends NSAction
  case object StageObserveComplete extends NSAction
  case object Done extends NSAction

  /** @group Typeclass Instances */
  implicit val NSActionEnumerated: Enumerated[NSAction] =
    Enumerated.of(Start,
                  NodStart,
                  NodComplete,
                  StageObserveStart,
                  StageObserveComplete,
                  Done)
}
