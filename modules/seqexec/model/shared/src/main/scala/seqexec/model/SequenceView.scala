// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.syntax.all._
import monocle.Traversal
import monocle.function.Each._
import monocle.macros.Lenses
import seqexec.model.Observation

@Lenses
final case class SequenceView (
  id:         Observation.Id,
  metadata:   SequenceMetadata,
  status:     SequenceState,
  steps:      List[Step],
  willStopIn: Option[Int]
) {

  def progress: Option[RunningStep] =
    RunningStep.fromInt(steps.count(_.isFinished), steps.length)

  // Returns where on the sequence the execution is at
  def runningStep: Option[RunningStep] = status match {
    case SequenceState.Running(_, _) => progress
    case SequenceState.Failed(_)     => progress
    case SequenceState.Aborted       => progress
    case _                           => none
  }
}

object SequenceView {
  implicit val eq: Eq[SequenceView] =
    Eq.by(x => (x.id, x.metadata, x.status, x.steps, x.willStopIn))

  val stepT: Traversal[SequenceView, Step] =
    SequenceView.steps ^|->> each
}
