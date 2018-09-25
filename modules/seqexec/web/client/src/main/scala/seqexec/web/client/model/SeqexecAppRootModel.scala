// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.enum.Site
import monocle.macros.Lenses
import seqexec.model.ClientID

/**
  * Root of the UI Model of the application
  */
@Lenses
final case class SeqexecAppRootModel(ws:       WebSocketConnection,
                                     site:     Option[Site],
                                     clientId: Option[ClientID],
                                     uiModel:  SeqexecUIModel)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SeqexecAppRootModel {
  val Initial: SeqexecAppRootModel = SeqexecAppRootModel(
    WebSocketConnection.Empty,
    None,
    None,
    SeqexecUIModel.Initial)

  val logDisplayL =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.globalLog ^|-> GlobalLog.display

  val sequencesOnDisplayL =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sequencesOnDisplay

  implicit val eq: Eq[SeqexecAppRootModel] =
    Eq.by(x => (x.ws, x.site, x.clientId, x.uiModel))
}
