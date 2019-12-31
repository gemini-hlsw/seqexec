// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import diode.data.Pot
import org.scalajs.dom.WebSocket
import scala.scalajs.js.timers.SetIntervalHandle

final case class WebSocketConnection(ws:            Pot[WebSocket],
                                     nextAttempt:   Int,
                                     autoReconnect: Boolean,
                                     pingInterval:  Option[SetIntervalHandle])

object WebSocketConnection {
  val Empty: WebSocketConnection =
    WebSocketConnection(diode.data.Empty, 0, autoReconnect = true, None)

  implicit val equal: Eq[WebSocketConnection] =
    Eq.by { x =>
      (x.ws, x.nextAttempt, x.autoReconnect)
    }

}
