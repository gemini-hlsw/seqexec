// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._
import seqexec.model.Observation

sealed trait UserPrompt extends Product with Serializable

object UserPrompt {
  sealed trait PromptButtonColor

  object PromptButtonColor {
    case object DefaultOk     extends PromptButtonColor
    case object DefaultCancel extends PromptButtonColor
    case object WarningOk     extends PromptButtonColor
    case object WarningCancel extends PromptButtonColor
  }

  implicit lazy val eq: Eq[UserPrompt] =
    Eq.instance {
      case (a: TargetCheckOverride, b: TargetCheckOverride) => a === b
      case _                                                => false
    }

  // UserPrompt whether to override the target check
  final case class TargetCheckOverride(sid: Observation.Id, stepId: StepId, obsTarget: String, tcsTarget: String)
      extends UserPrompt

  object TargetCheckOverride {
    implicit lazy val eq: Eq[TargetCheckOverride] =
      Eq.by(x => (x.sid, x.stepId, x.obsTarget, x.tcsTarget))
  }
}
