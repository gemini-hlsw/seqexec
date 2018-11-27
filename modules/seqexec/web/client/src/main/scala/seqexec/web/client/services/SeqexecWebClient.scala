// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.services

import boopickle.Default._
import cats.implicits._
import gem.Observation
import java.util.logging.LogRecord
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.XMLHttpRequest
import seqexec.model.ClientId
import seqexec.model.QueueId
import seqexec.model.Conditions
import seqexec.model.UserDetails
import seqexec.model.UserLoginRequest
import seqexec.model.Observer
import seqexec.model.Operator
import seqexec.model.Step
import seqexec.model.enum.CloudCover
import seqexec.model.enum.Instrument
import seqexec.model.enum.ImageQuality
import seqexec.model.enum.SkyBackground
import seqexec.model.enum.WaterVapor
import seqexec.model.enum.Resource
import seqexec.web.model.boopickle._
import seqexec.web.common.LogMessage
import seqexec.web.common.LogMessage._
import scala.scalajs.js.URIUtils._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.ArrayBuffer
import scala.scalajs.js.typedarray.TypedArrayBuffer

/**
  * Encapsulates remote calls to the Seqexec Web API
  */
@SuppressWarnings(
  Array(
    "org.wartremover.warts.Equals",
    "org.wartremover.warts.ImplicitParameter",
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.OptionPartial",
    "org.wartremover.warts.Throw"
  ))
object SeqexecWebClient extends ModelBooPicklers {
  private val baseUrl = "/api/seqexec"

  // Decodes the binary response with BooPickle, errors are not handled
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unpickle[A](r: XMLHttpRequest)(implicit u: Pickler[A]): A = {
    val ab = TypedArrayBuffer.wrap(r.response.asInstanceOf[ArrayBuffer])
    Unpickle[A].fromBytes(ab)
  }

  def sync(id: Observation.Id): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(id.format)}/sync",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to execute a sequence
    */
  def run(id: Observation.Id, clientId: ClientId): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(id.format)}/start/${encodeURI(clientId.self.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to set a breakpoint
    */
  def breakpoint(sid: Observation.Id, step: Step): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(sid.format)}/${step.id}/breakpoint/${step.breakpoint}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to set a breakpoint
    */
  def skip(sid: Observation.Id, step: Step): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(sid.format)}/${step.id}/skip/${step.skip}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to stop immediately this sequence
    */
  def stop(sid: Observation.Id, step: Int): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(sid.format)}/$step/stop",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to abort this sequenece
    */
  def abort(sid: Observation.Id, step: Int): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(sid.format)}/$step/abort",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to hold the current exposure
    */
  def pauseObs(sid: Observation.Id, step: Int): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(sid.format)}/$step/pauseObs",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to resume the current exposure
    */
  def resumeObs(sid: Observation.Id, step: Int): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(sid.format)}/$step/resumeObs",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to set the operator name of a sequence
    */
  def setOperator(name: Operator): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/operator/${encodeURI(name.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to set the observer name of a sequence
    */
  def setObserver(id: Observation.Id, name: String): Future[Unit] =
    Ajax
      .post(
        url =
          s"$baseUrl/commands/${encodeURI(id.format)}/observer/${encodeURI(name)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to set the Conditions globally
    */
  def setConditions(conditions: Conditions): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/conditions",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes(conditions)
      )
      .map(_ => ())

  /**
    * Requests the backend to set the ImageQuality
    */
  def setImageQuality(iq: ImageQuality): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/iq",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes[ImageQuality](iq)
      )
      .map(_ => ())

  /**
    * Requests the backend to set the CloudCover
    */
  def setCloudCover(cc: CloudCover): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/cc",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes[CloudCover](cc)
      )
      .map(_ => ())

  /**
    * Requests the backend to set the WaterVapor
    */
  def setWaterVapor(wv: WaterVapor): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/wv",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes[WaterVapor](wv)
      )
      .map(_ => ())

  /**
    * Requests the backend to set the SkyBackground
    */
  def setSkyBackground(sb: SkyBackground): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/sb",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes[SkyBackground](sb)
      )
      .map(_ => ())

  /**
    * Requests the backend to send a copy of the current state
    */
  def refresh(clientId: ClientId): Future[Unit] =
    Ajax
      .get(
        url          = s"$baseUrl/commands/refresh/${encodeURI(clientId.self.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to pause a sequence
    */
  def pause(id: Observation.Id): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(id.format)}/pause",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Requests the backend to cancel a pausing request in process
    */
  def cancelPause(id: Observation.Id): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/${encodeURI(id.format)}/cancelpause",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Login request
    */
  def login(u: String, p: String): Future[UserDetails] =
    Ajax
      .post(
        url          = s"$baseUrl/login",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes(UserLoginRequest(u, p))
      )
      .map(unpickle[UserDetails])

  /**
    * Logout request
    */
  def logout(): Future[String] =
    Ajax
      .post(
        url = s"$baseUrl/logout"
      )
      .map(_.responseText)

  /**
    * Load a sequence
    */
  def loadSequence(instrument: Instrument,
                   id:         Observation.Id,
                   name:       Observer,
                   clientId:   ClientId): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/load/${encodeURI(instrument.show)}/${encodeURI(id.format)}/${encodeURI(name.value)}/${encodeURI(clientId.self.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Log record
    */
  def log(record: LogRecord): Future[String] =
    Ajax
      .post(
        url          = s"$baseUrl/log",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes(LogMessage.fromLogRecord(record))
      )
      .map(_.responseText)

  /**
    * Read the site of the server
    */
  def site(): Future[String] =
    Ajax
      .post(
        url = s"$baseUrl/site"
      )
      .map(_.responseText)

  /**
    * Add a sequence from a queue
    */
  def removeSequenceFromQueue(queueId: QueueId,
                              id:      Observation.Id): Future[Unit] =
    Ajax
      .post(
        url =
          s"$baseUrl/commands/queue/${encodeURI(queueId.self.show)}/remove/${encodeURI(id.format)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Clears a queue
    */
  def clearQueue(queueId: QueueId): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/queue/${encodeURI(queueId.self.show)}/clear",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Runs a queue
    */
  def runQueue(queueId:  QueueId,
               clientId: ClientId,
               observer: Observer): Future[Unit] =
    Ajax
      .post(
        url =
          s"$baseUrl/commands/queue/${encodeURI(queueId.self.show)}/run/${encodeURI(
            observer.value)}/${encodeURI(clientId.self.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Stops a queue
    */
  def stopQueue(queueId: QueueId, clientId: ClientId): Future[Unit] =
    Ajax
      .post(
        url =
          s"$baseUrl/commands/queue/${encodeURI(queueId.self.show)}/stop/${encodeURI(clientId.self.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Add a sequence from a queue
    */
  def addSequencesToQueue(ids: List[Observation.Id])(qid: QueueId): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/queue/${encodeURI(qid.self.show)}/add",
        responseType = "arraybuffer",
        data         = Pickle.intoBytes(ids)
      )
      .map(_ => ())

  /**
    * Add a sequence from a queue
    */
  def addSequenceToQueue(id: Observation.Id)(qid: QueueId): Future[Unit] =
    Ajax
      .post(
        url          = s"$baseUrl/commands/queue/${encodeURI(qid.self.show)}/add/${encodeURI(id.format)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Stops a queue
    */
  def moveSequenceQueue(queueId: QueueId, obsId: Observation.Id, pos: Int, clientId: ClientId): Future[Unit] =
    Ajax
      .post(
        url =
          s"$baseUrl/commands/queue/${encodeURI(queueId.self.show)}/move/${encodeURI(obsId.self.format)}/$pos/${encodeURI(clientId.self.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

  /**
    * Runs a reusource
    */
  def runResource(pos: Int, resource: Resource)(obsId: Observation.Id): Future[Unit] =
    Ajax
      .post(
        url =
          s"$baseUrl/commands/execute/${encodeURI(obsId.self.format)}/$pos/${encodeURI(resource.show)}",
        responseType = "arraybuffer"
      )
      .map(_ => ())

}
