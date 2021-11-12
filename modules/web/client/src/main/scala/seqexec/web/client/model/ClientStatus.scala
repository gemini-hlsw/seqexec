// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import monocle.Getter
import monocle.Lens
import seqexec.model.UserDetails
import seqexec.model.ClientId
import seqexec.model.Observer

/**
 * Utility class to let components more easily switch parts of the UI depending on the user and
 * connection state
 */
final case class ClientStatus(
  user:         Option[UserDetails],
  clientId:     Option[ClientId],
  displayNames: Map[String, String],
  w:            WebSocketConnection
) {
  def isLogged: Boolean           = user.isDefined
  def isConnected: Boolean        = w.ws.isReady
  def canOperate: Boolean         = isLogged && isConnected
  def displayName: Option[String] = user.flatMap(u => displayNames.get(u.username))
  def observer: Option[Observer]  = displayName.map(Observer.apply)
}

object ClientStatus {
  implicit val eq: Eq[ClientStatus] =
    Eq.by(x => (x.user, x.clientId, x.displayNames, x.w))

  val clientStatusFocusL: Lens[SeqexecAppRootModel, ClientStatus] =
    Lens[SeqexecAppRootModel, ClientStatus](m =>
      ClientStatus(m.uiModel.user, m.clientId, m.uiModel.displayNames, m.ws)
    )(v =>
      m =>
        m.copy(ws = v.w,
               clientId = v.clientId,
               uiModel = m.uiModel.copy(user = v.user, displayNames = v.displayNames)
        )
    )

  val canOperateG: Getter[SeqexecAppRootModel, Boolean] =
    clientStatusFocusL.composeGetter(Getter[ClientStatus, Boolean](_.canOperate))
}
