// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import diode.data.Pot
import org.scalajs.dom.WebSocket

final case class WebSocketConnection(ws:            Pot[WebSocket],
                                     nextAttempt:   Int,
                                     autoReconnect: Boolean)

object WebSocketConnection {
  val Empty: WebSocketConnection =
    WebSocketConnection(diode.data.Empty, 0, autoReconnect = true)

  implicit val equal: Eq[WebSocketConnection] =
    Eq.by { x =>
      (x.ws, x.nextAttempt, x.autoReconnect)
    }

}
