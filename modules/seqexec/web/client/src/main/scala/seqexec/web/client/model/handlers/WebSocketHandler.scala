// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.handlers

import boopickle.DefaultBasic._
import cats.implicits._
import diode.util.RunAfterJS
import diode.{Action, ActionHandler, ActionResult, Effect, ModelRW, NoAction}
import diode.data.{Pending, Pot, Ready}
import java.util.logging.{Level, Logger}
import java.time.Instant
import mouse.all._
import org.scalajs.dom._
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import seqexec.model.boopickle.ModelBooPicklers
import seqexec.model.enum.ServerLogLevel
import seqexec.model.events._
import seqexec.web.client.model._
import seqexec.web.client.actions._
import seqexec.web.client.circuit._
import seqexec.web.client.services.log.ConsoleHandler

/**
  * Handles the WebSocket connection and performs reconnection if needed
  */
@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class WebSocketHandler[M](modelRW: ModelRW[M, WebSocketConnection]) extends ActionHandler(modelRW) with Handlers with ModelBooPicklers {

  private implicit val runner = new RunAfterJS
  private val logger = Logger.getLogger(this.getClass.getSimpleName)
  // Reconfigure to avoid sending ajax events in this logger
  logger.setUseParentHandlers(false)
  logger.addHandler(new ConsoleHandler(Level.FINE))

  // Makes a websocket connection and setups event listeners
  def webSocket: Future[Action] = Future[Action] {
    val host = document.location.host
    val protocol = document.location.protocol.startsWith("https").fold("wss", "ws")
    val url = s"$protocol://$host/api/seqexec/events"
    val ws = new WebSocket(url)

    def onOpen(): Unit = {
      logger.info(s"Connected to $url")
      SeqexecCircuit.dispatch(Connected(ws, 0))
    }

    def onMessage(e: MessageEvent): Unit = {
      e.data match {
        case buffer: ArrayBuffer =>
          val byteBuffer = TypedArrayBuffer.wrap(buffer)
          Either.catchNonFatal(Unpickle[SeqexecEvent].fromBytes(byteBuffer)) match {
            case Right(event: ServerLogMessage) =>
              SeqexecCircuit.dispatch(ServerMessage(event))
            case Right(event)                   =>
              logger.info(s"Decoding event: ${event.getClass}")
              SeqexecCircuit.dispatch(ServerMessage(event))
            case Left(t)                       =>
              logger.warning(s"Error decoding event ${t.getMessage}")
          }
        case _                   =>
          ()
      }
    }

    def onError(): Unit = logger.severe("Error on websocket")

    def onClose(): Unit =
      // Increase the delay to get exponential backoff with a minimum of 200ms and a max of 1m
      if (value.autoReconnect) {
        SeqexecCircuit.dispatch(ConnectionRetry(math.min(60000, math.max(200, value.nextAttempt * 2))))
      }

    ws.binaryType = "arraybuffer"
    ws.onopen = _ => onOpen
    ws.onmessage = onMessage _
    ws.onerror = _ => onError
    ws.onclose = _ => onClose
    Connecting
  }.recover {
    case _: Throwable => NoAction
  }

  def connectHandler: PartialFunction[Any, ActionResult[M]] = {
    case WSConnect(d) =>
      effectOnly(Effect(webSocket).after(d.millis))

    case Reconnect   =>
      // Capture the WS, or it maybe invalid during the Future
      val ws = value.ws
      val closeCurrent = Effect(Future(ws.foreach(_.close())).map(_ => NoAction))
      val reConnect = Effect(webSocket)
      updated(value.copy(ws = Pot.empty[WebSocket], nextAttempt = 0, autoReconnect = false), closeCurrent >> reConnect)
  }

  def connectingHandler: PartialFunction[Any, ActionResult[M]] = {
    case Connecting =>
      noChange
  }

  def connectedCloseHandler: PartialFunction[Any, ActionResult[M]] = {
    case WSClose =>
      // Forcefully close the websocket as requested when reloading the code via HMR
      val ws = value.ws
      val closeEffect = Effect(Future(ws.foreach(_.close())).map(_ => NoAction))
      updated(value.copy(ws = Pot.empty[WebSocket], nextAttempt = 0, autoReconnect = false), closeEffect)
  }

  def connectedHandler: PartialFunction[Any, ActionResult[M]] = {
    case Connected(ws, delay) =>
      updated(WebSocketConnection(Ready(ws), delay, autoReconnect = true))
  }

  def connectionErrorHandler: PartialFunction[Any, ActionResult[M]] = {
    case ConnectionError(_) =>
      effectOnly(Effect.action(AppendToLog(ServerLogMessage(ServerLogLevel.ERROR, Instant.now, "Error connecting to the seqexec server"))))
  }

  def connectionClosedHandler: PartialFunction[Any, ActionResult[M]] = {
    case ConnectionRetry(next) =>
      logger.fine(s"Retry connecting in $next")
      val effect = Effect(Future(WSConnect(next)))
      updated(value.copy(ws = Pending(), nextAttempt = next), effect)
  }

  // This is essentially a state machine to handle the connection status and
  // can reconnect if needed
  override def handle: PartialFunction[Any, ActionResult[M]] =
    List(connectHandler,
      connectingHandler,
      connectedHandler,
      connectionErrorHandler,
      connectedCloseHandler,
      connectionClosedHandler).combineAll
}
