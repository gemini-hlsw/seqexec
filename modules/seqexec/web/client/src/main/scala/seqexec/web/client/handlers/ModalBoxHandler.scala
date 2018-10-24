// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import cats.implicits._
import diode.{ Action, ActionHandler, ActionResult, ModelRW }
import seqexec.web.client.model._

/**
  * Handles actions related to opening/closing a modal
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals"))
class ModalBoxHandler[M](openAction: Action, closeAction: Action, modelRW: ModelRW[M, SectionVisibilityState]) extends ActionHandler(modelRW) with Handlers[M, SectionVisibilityState] {
  def openModal: PartialFunction[Any, ActionResult[M]] = {
    case x if x == openAction && value === SectionClosed =>
      updated(SectionOpen)

    case x if x == openAction                            =>
      noChange
  }

  def closeModal: PartialFunction[Any, ActionResult[M]] = {
    case x if x == closeAction && value === SectionOpen =>
      updated(SectionClosed)

    case x if x == closeAction                          =>
      noChange
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    openModal |+| closeModal
}
