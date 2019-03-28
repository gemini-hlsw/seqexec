// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.model.SequenceView
import seqexec.model.SequencesQueue
import seqexec.web.client.actions._
import seqexec.web.client.circuit._

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesFocus])
    extends ActionHandler(modelRW)
    with Handlers[M, SequencesFocus] {
  def handleSelectSequenceDisplay: PartialFunction[Any, ActionResult[M]] = {
    case SelectIdToDisplay(i, id, _) =>
      updatedL(
        SequencesFocus.sod.modify(_.focusOnSequence(i, id).hideStepConfig))

    case SelectSequencePreview(i, id, _) =>
      val seq = SequencesQueue
        .queueItemG[SequenceView](_.id === id)
        .get(value.sequences)
      seq
        .map { s =>
          updatedL(SequencesFocus.sod.modify(_.previewSequence(i, s)))
        }
        .getOrElse {
          noChange
        }

    case SelectCalibrationQueue =>
      updatedL(SequencesFocus.sod.modify(_.focusOnDayCal))

  }

  private def handleCalTabObserver: PartialFunction[Any, ActionResult[M]] = {
    case UpdateCalTabObserver(o) =>
      updatedL(SequencesFocus.sod.modify(_.updateCalTabObserver(o)))
  }

  private def handleShowHideStep: PartialFunction[Any, ActionResult[M]] = {
    case ShowPreviewStepConfig(i, id, step) =>
      val seq = SequencesQueue
        .queueItemG[SequenceView](_.id === id)
        .get(value.sequences)
      seq
        .map { s =>
          updatedL(
            SequencesFocus.sod.modify(
              _.previewSequence(i, s).showStepConfig(id, step - 1)))
        }
        .getOrElse {
          noChange
        }

    case ShowStepConfig(i, id, step) =>
      updatedL(
        SequencesFocus.sod.modify(
          _.focusOnSequence(i, id).showStepConfig(id, step - 1)))

  }

  private def handleClean: PartialFunction[Any, ActionResult[M]] = {
    case CleanSequences =>
      noChange
  }

  private def handleLoadFailed: PartialFunction[Any, ActionResult[M]] = {
    case SequenceLoadFailed(id) =>
      updatedL(SequencesFocus.sod.modify(_.loadingFailed(id)))
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleSelectSequenceDisplay,
         handleShowHideStep,
         handleLoadFailed,
         handleClean,
         handleCalTabObserver).combineAll
}
