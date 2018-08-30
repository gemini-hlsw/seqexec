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
class SequenceDisplayHandler[M](modelRW: ModelRW[M, SequencesFocus]) extends ActionHandler(modelRW) with Handlers {
  def handleSelectSequenceDisplay: PartialFunction[Any, ActionResult[M]] = {
    case SelectIdToDisplay(i, id, _) =>
      updated(SequencesFocus.sod.modify(_.focusOnSequence(i, id).hideStepConfig)(value))

    case SelectSequencePreview(i, id, _) =>
      val seq = SequencesQueue.queueItemG[SequenceView](_.id === id).get(value.sequences)
      updated(SequencesFocus.sod.modify(_.previewSequence(i, seq).hideStepConfig)(value))

    case SelectEmptyPreview =>
      updated(SequencesFocus.sod.modify(_.unsetPreview.focusOnPreview)(value))

  }

  def handleShowHideStep: PartialFunction[Any, ActionResult[M]] = {
    case ShowPreviewStepConfig(i, id, step) =>
      val seq = SequencesQueue.queueItemG[SequenceView](_.id === id).get(value.sequences)
      updated(SequencesFocus.sod.modify(_.previewSequence(i, seq).showStepConfig(id, step - 1))(value))

    case ShowStepConfig(i, id, step)        =>
      updated(SequencesFocus.sod.modify(_.focusOnSequence(i, id).showStepConfig(id, step - 1))(value))

  }

  def handleRememberCompleted: PartialFunction[Any, ActionResult[M]] = {
    case RememberCompleted(s) =>
      updated(SequencesFocus.sod.modify(_.markCompleted(s))(value))
  }

  def handleClean: PartialFunction[Any, ActionResult[M]] = {
    case CleanSequences =>
      updated(SequencesFocus.sod.modify(_.cleanAll)(value))
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleSelectSequenceDisplay,
      handleShowHideStep,
      handleRememberCompleted).combineAll
}
