// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.ActionHandler
import diode.ActionResult
import diode.ModelRW
import seqexec.web.client.actions._
import seqexec.web.client.model.SessionQueueFilter

/**
  * Handles updates to the session queue filter
  */
class SessionQueueFilterHandler[M](modelRW: ModelRW[M, SessionQueueFilter])
    extends ActionHandler(modelRW)
    with Handlers[M, SessionQueueFilter] {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateSessionFilter(mod) =>
      updatedL(mod)
  }
}
