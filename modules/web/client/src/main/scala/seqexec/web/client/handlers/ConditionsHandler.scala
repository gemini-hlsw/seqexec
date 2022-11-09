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
import seqexec.model.Conditions
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient

/**
 * Handles updates to conditions
 */
class ConditionsHandler[M](modelRW: ModelRW[M, Conditions])
    extends ActionHandler(modelRW)
    with Handlers[M, Conditions] {
  val iqHandle: PartialFunction[Any, ActionResult[M]] = { case UpdateImageQuality(iq) =>
    val updateE = Effect(SeqexecWebClient.setImageQuality(iq).as(NoAction))
    updated(value.copy(iq = iq), updateE)
  }

  val ccHandle: PartialFunction[Any, ActionResult[M]] = { case UpdateCloudCover(cc) =>
    val updateE = Effect(SeqexecWebClient.setCloudCover(cc).as(NoAction))
    updated(value.copy(cc = cc), updateE)
  }

  val sbHandle: PartialFunction[Any, ActionResult[M]] = { case UpdateSkyBackground(sb) =>
    val updateE = Effect(SeqexecWebClient.setSkyBackground(sb).as(NoAction))
    updated(value.copy(sb = sb), updateE)
  }

  val wvHandle: PartialFunction[Any, ActionResult[M]] = { case UpdateWaterVapor(wv) =>
    val updateE = Effect(SeqexecWebClient.setWaterVapor(wv).as(NoAction))
    updated(value.copy(wv = wv), updateE)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    iqHandle |+| ccHandle |+| sbHandle |+| wvHandle
}
