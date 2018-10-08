// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.enum.Site
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model._
import seqexec.web.client.model.Pages
import seqexec.web.client.model.SeqexecAppRootModel

@Lenses
final case class WebSocketsFocus(location:        Pages.SeqexecPages,
                                 sequences:       SequencesQueue[SequenceView],
                                 user:            Option[UserDetails],
                                 defaultObserver: Observer,
                                 clientId:        Option[ClientID],
                                 site:            Option[Site])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object WebSocketsFocus {
  implicit val eq: Eq[WebSocketsFocus] =
    Eq.by(x =>
      (x.location, x.sequences, x.user, x.defaultObserver, x.clientId, x.site))

  val webSocketFocusL: Lens[SeqexecAppRootModel, WebSocketsFocus] =
    Lens[SeqexecAppRootModel, WebSocketsFocus](
      m =>
        WebSocketsFocus(m.uiModel.navLocation,
                        m.sequences,
                        m.uiModel.user,
                        m.uiModel.defaultObserver,
                        m.clientId,
                        m.site))(
      v =>
        m =>
          m.copy(sequences = v.sequences,
                 uiModel = m.uiModel.copy(user = v.user,
                                          defaultObserver = v.defaultObserver),
                 clientId = v.clientId,
                 site     = v.site))
}
