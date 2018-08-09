// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import diode.{ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import seqexec.model.Conditions
import seqexec.web.client.actions._
import seqexec.web.client.services.SeqexecWebClient
import cats.implicits._

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

/**
 * Handles updates to conditions
 */
class ConditionsHandler[M](modelRW: ModelRW[M, Conditions]) extends ActionHandler(modelRW) with Handlers {
  val iqHandle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateImageQuality(iq) =>
      val updateE = Effect(SeqexecWebClient.setImageQuality(iq).map(_ => NoAction))
      val updatedSequences = value.copy(iq = iq)
      updated(updatedSequences, updateE)
  }

  val ccHandle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateCloudCover(cc) =>
      val updateE = Effect(SeqexecWebClient.setCloudCover(cc).map(_ => NoAction))
      val updatedSequences = value.copy(cc = cc)
      updated(updatedSequences, updateE)
  }

  val sbHandle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateSkyBackground(sb) =>
      val updateE = Effect(SeqexecWebClient.setSkyBackground(sb).map(_ => NoAction))
      val updatedSequences = value.copy(sb = sb)
      updated(updatedSequences, updateE)
  }

  val wvHandle: PartialFunction[Any, ActionResult[M]] = {
    case UpdateWaterVapor(wv) =>
      val updateE = Effect(SeqexecWebClient.setWaterVapor(wv).map(_ => NoAction))
      val updatedSequences = value.copy(wv = wv)
      updated(updatedSequences, updateE)
  }

  override def handle: PartialFunction[Any, ActionResult[M]] =
    iqHandle |+| ccHandle |+| sbHandle |+| wvHandle
}
