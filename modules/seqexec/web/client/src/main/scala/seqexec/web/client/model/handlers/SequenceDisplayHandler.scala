// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ActionHandler, ActionResult, ModelRW}
import seqexec.web.client.model._
import seqexec.web.client.actions._
import seqexec.web.client.circuit._

/**
  * Handles actions related to the changing the selection of the displayed sequence
  */
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesOnDisplay]) extends ActionHandler(modelRW) with Handlers {
  def handleSelectSequenceDisplay: PartialFunction[Any, ActionResult[M]] = {
    case SelectIdToDisplay(i, id) =>
      updated(value.focusOnSequence(i, id).hideStepConfig)

    case SelectSequencePreview(i, id, _) =>
      val seq = SeqexecCircuit.sequenceRef(id)
      updated(value.previewSequence(i, seq).hideStepConfig)

    case SelectEmptyPreview =>
      updated(value.unsetPreview.focusOnPreview)

  }

  def handleShowHideStep: PartialFunction[Any, ActionResult[M]] = {
    case ShowPreviewStepConfig(i, id, step) =>
      val seq = SeqexecCircuit.sequenceRef(id)
      updated(value.previewSequence(i, seq).showStepConfig(id, step - 1))

    case ShowStepConfig(i, id, step)        =>
      updated(value.focusOnSequence(i, id).showStepConfig(id, step - 1))

    case HideStepConfig(instrument)         =>
      if (value.sequences.focus.sequence.exists(_.metadata.instrument == instrument)) {
        updated(value.hideStepConfig)
      } else {
        noChange
      }
  }

  def handleRememberCompleted: PartialFunction[Any, ActionResult[M]] = {
    case RememberCompleted(s) =>
      updated(value.markCompleted(s))
  }

  def handleClean: PartialFunction[Any, ActionResult[M]] = {
    case CleanSequences =>
      updated(value.cleanAll)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleSelectSequenceDisplay,
      handleShowHideStep,
      handleRememberCompleted).combineAll
}
