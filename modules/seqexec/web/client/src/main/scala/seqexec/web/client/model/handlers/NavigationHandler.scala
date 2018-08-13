// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ActionHandler, ActionResult, Effect, ModelRW}
import seqexec.web.client.model._
import seqexec.web.client.model.Pages._
import seqexec.web.client.actions._
import scala.concurrent.Future
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

class NavigationHandler[M](modelRW: ModelRW[M, Pages.SeqexecPages]) extends ActionHandler(modelRW) with Handlers {
  def handleNavigateTo: PartialFunction[Any, ActionResult[M]] = {
    case NavigateTo(page) =>
      updated(page)
  }

  def handleSilentTo: PartialFunction[Any, ActionResult[M]] = {
    case NavigateSilentTo(page) =>
      val effect = page match {
        case InstrumentPage(i)               =>
          Effect(Future(SelectInstrumentToDisplay(i)))
        case SequencePage(_, id, _)          =>
          Effect(Future(SelectIdToDisplay(id)))
        case SequenceConfigPage(_, id, step) =>
          Effect(Future(ShowStepConfig(id, step, false)))
        case PreviewPage(_, id, step) =>
          Effect(Future(SelectSequencePreview(id, step)))
        case PreviewConfigPage(_, id, step) =>
          Effect(Future(ShowStepConfig(id, step, true)))
        case EmptyPreviewPage =>
          Effect(Future(SelectEmptyPreview))
        case _                               =>
          VoidEffect
      }
      updatedSilent(page, effect)
  }

  def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleNavigateTo, handleSilentTo).combineAll
}
