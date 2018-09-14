// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ActionHandler, ActionResult, ModelRW}
import seqexec.model.{ SequencesQueue, SequenceView }
import seqexec.web.client.actions._
import seqexec.web.client.circuit._

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesFocus]) extends ActionHandler(modelRW) with Handlers[M, SequencesFocus] {
  def handleSelectSequenceDisplay: PartialFunction[Any, ActionResult[M]] = {
    case SelectIdToDisplay(i, id, _) =>
      updatedL(SequencesFocus.sod.modify(_.focusOnSequence(i, id).hideStepConfig))

    case SelectSequencePreview(i, id, _) =>
      val seq = SequencesQueue.queueItemG[SequenceView](_.id === id).get(value.sequences)
      updatedL(SequencesFocus.sod.modify(_.previewSequence(i, seq)))

    case SelectEmptyPreview =>
      updatedL(SequencesFocus.sod.modify(_.unsetPreview.focusOnPreview))

    case SelectCalibrationQueue =>
      updatedL(SequencesFocus.sod.modify(_.focusOnDayCal))

  }

  def handleShowHideStep: PartialFunction[Any, ActionResult[M]] = {
    case ShowPreviewStepConfig(i, id, step) =>
      val seq = SequencesQueue.queueItemG[SequenceView](_.id === id).get(value.sequences)
      updatedL(SequencesFocus.sod.modify(_.previewSequence(i, seq).showStepConfig(id, step - 1)))

    case ShowStepConfig(i, id, step)        =>
      updatedL(SequencesFocus.sod.modify(_.focusOnSequence(i, id).showStepConfig(id, step - 1)))

  }

  def handleRememberCompleted: PartialFunction[Any, ActionResult[M]] = {
    case RememberCompleted(s) =>
      updatedL(SequencesFocus.sod.modify(_.markCompleted(s)))
  }

  def handleClean: PartialFunction[Any, ActionResult[M]] = {
    case CleanSequences =>
      updatedL(SequencesFocus.sod.modify(_.cleanAll))
  }

  def handleLoadFailed: PartialFunction[Any, ActionResult[M]] = {
    case SequenceLoadFailed(id) =>
      updatedL(SequencesFocus.sod.modify(_.loadingComplete(id)))
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleSelectSequenceDisplay,
      handleShowHideStep,
      handleLoadFailed,
      handleRememberCompleted).combineAll
}
