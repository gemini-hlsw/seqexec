// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats._
import cats.syntax.all._
import monocle.Getter
import monocle.macros.Lenses
import seqexec.model.Observation
import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.model._
import web.client.table._

@Lenses
final case class StepsTableFocus(id:                  Observation.Id,
                                 instrument:          Instrument,
                                 state:               SequenceState,
                                 steps:               List[Step],
                                 stepConfigDisplayed: Option[Int],
                                 nextStepToRun:       Option[StepId],
                                 selectedStep:        Option[StepId],
                                 runningStep:         Option[RunningStep],
                                 isPreview:           Boolean,
                                 tableState:          TableState[StepsTable.TableColumn],
                                 tabOperations:       TabOperations)

object StepsTableFocus {
  implicit val eq: Eq[StepsTableFocus] =
    Eq.by(
      x =>
        (x.id,
         x.instrument,
         x.state,
         x.steps,
         x.stepConfigDisplayed,
         x.nextStepToRun,
         x.selectedStep,
         x.runningStep,
         x.isPreview,
         x.tableState,
         x.tabOperations))

  def stepsTableG(
    id: Observation.Id
  ): Getter[SeqexecAppRootModel, Option[StepsTableFocus]] =
    SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
      SequencesOnDisplay.tabG(id)).zip(SeqexecAppRootModel.stepsTableStateL(id).asGetter) >>> {
        case (Some(SeqexecTabActive(tab, _)), ts) =>
          val sequence = tab.sequence
          StepsTableFocus(
            sequence.id,
            sequence.metadata.instrument,
            sequence.status,
            sequence.steps,
            tab.stepConfigDisplayed,
            sequence.nextStepToRun,
            tab.selectedStep
              .orElse(sequence.nextStepToRun), // start with the nextstep selected
            sequence.runningStep,
            tab.isPreview,
            ts.getOrElse(StepsTable.State.InitialTableState),
            tab.tabOperations
          ).some
        case _ => none
    }
}
