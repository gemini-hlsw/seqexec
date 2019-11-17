// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.Getter
import monocle.Lens
import seqexec.model.{Observer, UserDetails}

/**
  * Utility class to let components more easily switch parts of the UI depending on the user and connection state
  */
final case class ClientStatus(
  userDetails: Option[UserDetails],
  defaultObserver: Observer,
  webSocket: WebSocketConnection
) {
  def isLogged: Boolean    = userDetails.isDefined
  def isConnected: Boolean = webSocket.ws.isReady
  def canOperate: Boolean  = isLogged && isConnected
}

object ClientStatus {
  implicit val eq: Eq[ClientStatus] =
    Eq.by(x => (x.userDetails, x.defaultObserver, x.webSocket))

  val clientStatusFocusL: Lens[SeqexecAppRootModel, ClientStatus] =
    Lens[SeqexecAppRootModel, ClientStatus](m =>
      ClientStatus(m.uiModel.user, m.uiModel.defaultObserver, m.ws))(v =>
      m => m.copy(ws = v.webSocket, uiModel = m.uiModel.copy(user = v.userDetails, defaultObserver = v.defaultObserver)))

  val canOperateG: Getter[SeqexecAppRootModel, Boolean] =
    clientStatusFocusL.composeGetter(
      Getter[ClientStatus, Boolean](_.canOperate))
}
