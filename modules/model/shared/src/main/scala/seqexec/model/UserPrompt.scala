// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.data.NonEmptyList
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

  sealed trait SeqCheck extends Product with Serializable;

  object SeqCheck {
    implicit lazy val eq: Eq[SeqCheck] = Eq.instance {
      case (a: TargetCheckOverride, b: TargetCheckOverride)               => a === b
      case (a: ObsConditionsCheckOverride, b: ObsConditionsCheckOverride) => a === b
      case _                                                              => false
    }
  }

  final case class Discrepancy[A](actual: A, required: A)

  object Discrepancy {
    implicit def eq[A: Eq]: Eq[Discrepancy[A]] = Eq.by(x => (x.actual, x.required))
  }

  final case class TargetCheckOverride(self: Discrepancy[String]) extends SeqCheck

  object TargetCheckOverride {
    implicit lazy val eq: Eq[TargetCheckOverride] =
      Eq.by(_.self)
  }

  // UserPrompt whether to override the observing conditions
  final case class ObsConditionsCheckOverride(
    cc: Option[Discrepancy[String]],
    iq: Option[Discrepancy[String]],
    sb: Option[Discrepancy[String]],
    wv: Option[Discrepancy[String]]
  ) extends SeqCheck

  object ObsConditionsCheckOverride {
    implicit lazy val eq: Eq[ObsConditionsCheckOverride] = Eq.by(x => (x.cc, x.iq, x.sb, x.wv))
  }

  implicit lazy val eq: Eq[UserPrompt] =
    Eq.instance {
      case (a: ChecksOverride, b: ChecksOverride) => a === b
      case _                                      => false
    }

  // UserPrompt whether to override start checks
  final case class ChecksOverride(
    sid:    Observation.Id,
    stepId: StepId,
    checks: NonEmptyList[SeqCheck]
  ) extends UserPrompt

  object ChecksOverride {
    implicit lazy val eq: Eq[ChecksOverride] = Eq.by(x => (x.sid, x.stepId, x.checks))
  }

}
