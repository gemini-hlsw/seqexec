// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import gem.Observation

sealed trait UserPrompt extends Product with Serializable

object UserPrompt {
  implicit lazy val eq: Eq[UserPrompt] =
    Eq.instance {
      case (a: TargetCheckOverride, b: TargetCheckOverride) => a === b
      case _                                                => false
    }

  def header(n: UserPrompt): String =
    n match {
      case TargetCheckOverride(_) => "Target check failed"
    }

  def body(n: UserPrompt): List[String] =
    n match {
      case TargetCheckOverride(sid) =>
        List(
          s"Target on TCS and sequenc differ for obsid: '${sid.format}'"
        )
    }

  // UserPrompt whether to override the target check
  final case class TargetCheckOverride(sid: Observation.Id) extends UserPrompt

  object TargetCheckOverride {
    implicit lazy val eq: Eq[TargetCheckOverride] =
      Eq.by(_.sid)
  }
}
