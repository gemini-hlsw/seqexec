// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import diode.data.Pot
import gem.enum.Site
import monocle.macros.Lenses
import org.scalajs.dom.WebSocket
import seqexec.model.ClientID
import seqexec.model.events._
import seqexec.web.common.FixedLengthBuffer

final case class RunningStep(last: Int, total: Int)

object RunningStep {
  implicit val show: Show[RunningStep] =
    Show.show(u => s"${u.last + 1}/${u.total}")

  implicit val eq: Eq[RunningStep] =
    Eq.by(x => (x.last, x.total))
}

// UI model
sealed trait SectionVisibilityState extends Product with Serializable
case object SectionOpen extends SectionVisibilityState
case object SectionClosed extends SectionVisibilityState

object SectionVisibilityState {
  implicit val eq: Eq[SectionVisibilityState] = Eq.fromUniversalEquals
}

final case class WebSocketConnection(ws: Pot[WebSocket], nextAttempt: Int, autoReconnect: Boolean)

object WebSocketConnection {
  val Empty: WebSocketConnection = WebSocketConnection(diode.data.Empty, 0, autoReconnect = true)

  implicit val equal: Eq[WebSocketConnection] =
    Eq.by { x =>
      (x.ws, x.nextAttempt, x.autoReconnect)
    }

}

/**
  * Keeps a list of log entries for display
  */
final case class GlobalLog(log: FixedLengthBuffer[ServerLogMessage], display: SectionVisibilityState)

object GlobalLog {
  implicit val eq: Eq[GlobalLog] = Eq.by(x => (x.log, x.display))
}

/**
  * Root of the UI Model of the application
  */
@Lenses
final case class SeqexecAppRootModel(ws: WebSocketConnection, site: Option[Site], clientId: Option[ClientID], uiModel: SeqexecUIModel)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SeqexecAppRootModel {
  val Initial: SeqexecAppRootModel = SeqexecAppRootModel(WebSocketConnection.Empty, None, None, SeqexecUIModel.Initial)
}
