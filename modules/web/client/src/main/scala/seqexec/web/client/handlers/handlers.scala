// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import cats.syntax.all._
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.ModelRW
import diode.NoAction
import lucuma.core.enums.Site
import seqexec.model.Operator
import seqexec.web.client.actions._
import seqexec.web.client.model.GlobalLog
import seqexec.web.client.services.SeqexecWebClient
import seqexec.web.client.services.DisplayNamePersistence

/**
 * Handles updates to the operator
 */
class OperatorHandler[M](modelRW: ModelRW[M, Option[Operator]])
    extends ActionHandler(modelRW)
    with Handlers[M, Option[Operator]] {
  override def handle: PartialFunction[Any, ActionResult[M]] = { case UpdateOperator(name) =>
    val updateOperatorE = Effect(SeqexecWebClient.setOperator(name).as(NoAction))
    updated(name.some, updateOperatorE)
  }
}

/**
 * Handles updates to the operator
 */
class DisplayNameHandler[M](modelRW: ModelRW[M, Map[String, String]])
    extends ActionHandler(modelRW)
    with Handlers[M, Map[String, String]]
    with DisplayNamePersistence {

  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateDisplayName(username, dn) =>
      val result    = value + (username -> dn)
      val persistDN = Effect(persistDisplayName(result).as(NoAction))
      updated(result, persistDN)
  }
}

/**
 * Handles setting the site
 */
class SiteHandler[M](modelRW: ModelRW[M, Option[Site]])
    extends ActionHandler(modelRW)
    with Handlers[M, Option[Site]] {

  override def handle: PartialFunction[Any, ActionResult[M]] = { case Initialize(site) =>
    updated(Some(site))
  }
}

/**
 * Handles updates to the log
 */
class GlobalLogHandler[M](modelRW: ModelRW[M, GlobalLog])
    extends ActionHandler(modelRW)
    with Handlers[M, GlobalLog] {
  override def handle: PartialFunction[Any, ActionResult[M]] = {
    case AppendToLog(s) =>
      updated(value.copy(log = value.log.append(s)))

    case ToggleLogArea =>
      updated(value.copy(display = value.display.toggle))
  }
}
