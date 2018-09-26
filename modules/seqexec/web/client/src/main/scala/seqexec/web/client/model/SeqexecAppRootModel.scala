// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.enum.Site
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model.Conditions
import seqexec.model.SequenceView
import seqexec.model.SequencesQueue
import seqexec.model.ClientID

/**
  * Root of the UI Model of the application
  */
@Lenses
final case class SeqexecAppRootModel(sequences: SequencesQueue[SequenceView],
                                     ws:        WebSocketConnection,
                                     site:      Option[Site],
                                     clientId:  Option[ClientID],
                                     uiModel:   SeqexecUIModel)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SeqexecAppRootModel {
  private val NoSequencesLoaded: SequencesQueue[SequenceView] =
    SequencesQueue[SequenceView](Map.empty, Conditions.Default, None, Nil)

  val Initial: SeqexecAppRootModel = SeqexecAppRootModel(
    NoSequencesLoaded,
    WebSocketConnection.Empty,
    None,
    None,
    SeqexecUIModel.Initial)

  val logDisplayL: Lens[SeqexecAppRootModel, SectionVisibilityState] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.globalLog ^|-> GlobalLog.display

  val sequencesOnDisplayL: Lens[SeqexecAppRootModel, SequencesOnDisplay] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.sequencesOnDisplay

  implicit val eq: Eq[SeqexecAppRootModel] =
    Eq.by(x => (x.sequences, x.ws, x.site, x.clientId, x.uiModel))
}
