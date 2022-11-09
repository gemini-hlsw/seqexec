// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.syntax.all._
import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.web.client.actions._
import seqexec.web.client.model._

class NavigationHandler[M](modelRW: ModelRW[M, Pages.SeqexecPages])
    extends ActionHandler(modelRW)
    with Handlers[M, Pages.SeqexecPages] {
  def handleNavigateTo: PartialFunction[Any, ActionResult[M]] = { case NavigateTo(page) =>
    updated(page)
  }

  def handleSilentTo: PartialFunction[Any, ActionResult[M]] = { case NavigateSilentTo(page) =>
    updatedSilent(page)
  }

  def handle: PartialFunction[Any, ActionResult[M]] =
    List(handleNavigateTo, handleSilentTo).combineAll
}
