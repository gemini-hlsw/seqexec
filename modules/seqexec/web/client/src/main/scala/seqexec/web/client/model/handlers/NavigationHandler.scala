// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ActionHandler, ActionResult, /*Effect,*/ ModelRW}
import seqexec.web.client.model._
import seqexec.web.client.actions._

class NavigationHandler[M](modelRW: ModelRW[M, Pages.SeqexecPages]) extends ActionHandler(modelRW) with Handlers {
  def handleNavigateTo: PartialFunction[Any, ActionResult[M]] = {
    case NavigateTo(page) =>
      updated(page)
  }

  def handleSilentTo: PartialFunction[Any, ActionResult[M]] = {
    case NavigateSilentTo(page) =>
      updatedSilent(page)
  }

  def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleNavigateTo, handleSilentTo).combineAll
}
