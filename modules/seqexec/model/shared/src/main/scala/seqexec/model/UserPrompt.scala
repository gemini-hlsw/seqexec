// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import gem.Observation

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

  def title(n: UserPrompt): String =
    n match {
      case _: TargetCheckOverride => "Warning!"
    }

  def okButton(n: UserPrompt): String =
    n match {
      case _: TargetCheckOverride => "Stop"
    }

  def cancelButton(n: UserPrompt): String =
    n match {
      case _: TargetCheckOverride => "Continue anyway"
    }

  def okColor(n: UserPrompt): PromptButtonColor =
    n match {
      case _: TargetCheckOverride => PromptButtonColor.DefaultOk
    }

  def cancelColor(n: UserPrompt): PromptButtonColor =
    n match {
      case _: TargetCheckOverride => PromptButtonColor.WarningCancel
    }

  def question(n: UserPrompt): List[String] =
    n match {
      case TargetCheckOverride(sid, obsTarget, tcsTarget) =>
        List(s"There is a target mismatch running sequence ${sid.format}",
             s"Target in the sequence: ${obsTarget}",
             s"Target in the TCS: ${tcsTarget}"
        )
    }

  implicit class UserPromptOps(val p: UserPrompt) extends AnyVal {
    def question: List[String]         = UserPrompt.question(p)
    def title: String                  = UserPrompt.title(p)
    def okButton: String               = UserPrompt.okButton(p)
    def cancelButton: String           = UserPrompt.cancelButton(p)
    def okColor: PromptButtonColor     = UserPrompt.okColor(p)
    def cancelColor: PromptButtonColor = UserPrompt.cancelColor(p)
  }

  // UserPrompt whether to override the target check
  final case class TargetCheckOverride(sid: Observation.Id, obsTarget: String, tcsTarget: String)
      extends UserPrompt

  object TargetCheckOverride {
    implicit lazy val eq: Eq[TargetCheckOverride] =
      Eq.by(x => (x.sid, x.obsTarget, x.tcsTarget))
  }
}
