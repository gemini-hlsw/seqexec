// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.syntax.all._

sealed trait RunningStep {
  val last: StepId
  val total: StepId
}

object RunningStep {
  val Zero: RunningStep = RunningStepImpl(0, 0)

  private final case class RunningStepImpl(last: StepId, total: StepId) extends RunningStep

  def fromInt(last: StepId, total: StepId): Option[RunningStep] =
    if (last >= 0 && total >= last) RunningStepImpl(last, total).some else none

  def unapply(r: RunningStep): Option[(StepId, StepId)] =
    Some((r.last, r.total))

  implicit val show: Show[RunningStep] =
    Show.show(u => s"${u.last + 1}/${u.total}")

  implicit val eq: Eq[RunningStep] =
    Eq.by(x => (x.last, x.total))
}
