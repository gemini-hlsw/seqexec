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
    case SelectInstrumentToDisplay(_) =>
    println("select")
      noChange
      // updated(value.copy(_1 = value._1.focusOnInstrument(i)))

    case SelectIdToDisplay(id) =>
      updated(value.focusOnSequence(id))

    case SelectSequencePreview(id, _) =>
      val seq = SeqexecCircuit.sequenceRef(id)
      updated(value.previewSequence(seq))

    case SelectEmptyPreview =>
      updated(value.unsetPreview.focusOnPreview)

  }

  // def handleInitialize: PartialFunction[Any, ActionResult[M]] = {
  //   case Initialize(site) =>
  //     updated(value.copy(_2 = Some(site)))
  // }

  def handleShowHideStep: PartialFunction[Any, ActionResult[M]] = {
    case ShowStepConfig(id, step, true) =>
      val seq = SeqexecCircuit.sequenceRef(id)
      updated(value.previewSequence(seq).showStepConfig(step - 1))

    case ShowStepConfig(id, step, false) =>
      updated(value.focusOnSequence(id).showStepConfig(step - 1))

    case HideStepConfig(instrument) =>
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

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleSelectSequenceDisplay,
      handleShowHideStep,
      handleRememberCompleted).combineAll
}
