// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.services

import java.util.logging.LogRecord

import edu.gemini.seqexec.model.{ModelBooPicklers, UserDetails, UserLoginRequest}
import edu.gemini.seqexec.model.Model.{Conditions, CloudCover, ImageQuality, SkyBackground, WaterVapor, SequencesQueue, SequenceId}
import edu.gemini.seqexec.web.common._
import edu.gemini.seqexec.web.common.LogMessage._
import org.scalajs.dom.ext.{Ajax, AjaxException}
import org.scalajs.dom.XMLHttpRequest
import boopickle.Default._
import edu.gemini.seqexec.model.Model.Step
import scala.scalajs.js.URIUtils._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}

/**
  * Encapsulates remote calls to the Seqexec Web API
  */
@SuppressWarnings(Array("org.wartremover.warts.Equals", "org.wartremover.warts.ImplicitParameter", "org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.OptionPartial", "org.wartremover.warts.Throw"))
object SeqexecWebClient extends ModelBooPicklers {
  private val baseUrl = "/api/seqexec"

  // Decodes the binary response with BooPickle, errors are not handled
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unpickle[A](r: XMLHttpRequest)(implicit u: Pickler[A]): A = {
    val ab = TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer])
    Unpickle[A].fromBytes(ab)
  }

  def sync(id: SequenceId): Future[SequencesQueue[SequenceId]] =
    Ajax.get(
      url = s"$baseUrl/sequence/$id",
      responseType = "arraybuffer"
    )
    .map(unpickle[SequencesQueue[SequenceId]])
    .recover {
      case AjaxException(xhr) if xhr.status == HttpStatusCodes.NotFound  =>
        // If not found, we'll consider it like an empty response
        SequencesQueue(Conditions.default, None, Nil)
    }

  /**
    * Requests the backend to execute a sequence
    */
  def run(id: SequenceId): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/$id/start",
      responseType = "arraybuffer"
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set a breakpoint
    */
  def breakpoint(sid: SequenceId, step: Step): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/$sid/${step.id}/breakpoint/${step.breakpoint}",
      responseType = "arraybuffer"
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set the operator name of a sequence
    */
  def setOperator(name: String): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/operator/${encodeURI(name)}",
      responseType = "arraybuffer"
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set the observer name of a sequence
    */
  def setObserver(id: SequenceId, name: String): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/$id/observer/${encodeURI(name)}",
      responseType = "arraybuffer"
          ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set the Conditions globally
    */
  def setConditions(conditions: Conditions): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/conditions",
      responseType = "arraybuffer",
      data = Pickle.intoBytes(conditions)
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set the ImageQuality
    */
  def setImageQuality(iq: ImageQuality): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/iq",
      responseType = "arraybuffer",
      data = Pickle.intoBytes[ImageQuality](iq)
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set the CloudCover
    */
  def setCloudCover(cc: CloudCover): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/cc",
      responseType = "arraybuffer",
      data = Pickle.intoBytes[CloudCover](cc)
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set the WaterVapor
    */
  def setWaterVapor(wv: WaterVapor): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/wv",
      responseType = "arraybuffer",
      data = Pickle.intoBytes[WaterVapor](wv)
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to set the SkyBackground
    */
  def setSkyBackground(sb: SkyBackground): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/sb",
      responseType = "arraybuffer",
      data = Pickle.intoBytes[SkyBackground](sb)
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to send a copy of the current state
    */
  def refresh(): Future[RegularCommand] = {
    Ajax.get(
      url = s"$baseUrl/commands/refresh",
      responseType = "arraybuffer"
    ).map(unpickle[RegularCommand])
  }

  /**
    * Requests the backend to stop a sequence
    */
  def stop(id: SequenceId): Future[RegularCommand] = {
    Ajax.post(
      url = s"$baseUrl/commands/$id/pause",
      responseType = "arraybuffer"
    ).map(unpickle[RegularCommand])
  }

  /**
    * Login request
    */
  def login(u: String, p: String): Future[UserDetails] =
    Ajax.post(
      url = s"$baseUrl/login",
      responseType = "arraybuffer",
      data = Pickle.intoBytes(UserLoginRequest(u, p))
    ).map(unpickle[UserDetails])

  /**
    * Logout request
    */
  def logout(): Future[String] =
    Ajax.post(
      url = s"$baseUrl/logout"
    ).map(_.responseText)

  /**
    * Log record
    */
  def log(record: LogRecord): Future[String] =
    Ajax.post(
      url = s"$baseUrl/log",
      responseType = "arraybuffer",
      data = Pickle.intoBytes(LogMessage.fromLogRecord(record))
    ).map(_.responseText)
}
