// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import scala.collection.immutable.SortedMap

import cats._
import cats.implicits._
import lucuma.core.enum.Site
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model.Observation
import seqexec.model._
import seqexec.model.enum.Resource
import seqexec.web.client.model.AlignAndCalibStep
import seqexec.web.client.model.Pages
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.model.SeqexecAppRootModel
import seqexec.web.client.model.SequenceTab
import seqexec.web.client.model.SequencesOnDisplay
import seqexec.web.client.model.SoundSelection

@Lenses
final case class WebSocketsFocus(
  location:             Pages.SeqexecPages,
  sequences:            SequencesQueue[SequenceView],
  resourceRunRequested: Map[Observation.Id, SortedMap[Resource, ResourceRunOperation]],
  user:                 Option[UserDetails],
  displayNames:         Map[String, String],
  clientId:             Option[ClientId],
  site:                 Option[Site],
  sound:                SoundSelection,
  serverVersion:        Option[String],
  guideConfig:          TelescopeGuideConfig,
  alignAndCalib:        AlignAndCalibStep
)

object WebSocketsFocus {
  implicit val eq: Eq[WebSocketsFocus] =
    Eq.by(x =>
      (x.location,
       x.sequences,
       x.user,
       x.displayNames,
       x.clientId,
       x.site,
       x.serverVersion,
       x.guideConfig,
       x.alignAndCalib
      )
    )

  val webSocketFocusL: Lens[SeqexecAppRootModel, WebSocketsFocus] =
    Lens[SeqexecAppRootModel, WebSocketsFocus](m =>
      WebSocketsFocus(
        m.uiModel.navLocation,
        m.sequences,
        SeqexecAppRootModel.sequenceTabsT
          .getAll(m)
          .map(t => t.obsId -> t.tabOperations.resourceRunRequested)
          .toMap,
        m.uiModel.user,
        m.uiModel.displayNames,
        m.clientId,
        m.site,
        m.uiModel.sound,
        m.serverVersion,
        m.guideConfig,
        m.alignAndCalib
      )
    )(v =>
      m =>
        m.copy(
          sequences = v.sequences,
          uiModel = m.uiModel.copy(
            user = v.user,
            sequencesOnDisplay = SequencesOnDisplay.sequenceTabs.modify(seqTab =>
              SequenceTab.resourcesRunOperationsL.set(
                v.resourceRunRequested
                  .getOrElse(seqTab.obsId, SortedMap.empty)
              )(seqTab)
            )(m.uiModel.sequencesOnDisplay),
            displayNames = v.displayNames,
            sound = v.sound
          ),
          clientId = v.clientId,
          site = v.site,
          serverVersion = v.serverVersion,
          guideConfig = v.guideConfig,
          alignAndCalib = v.alignAndCalib
        )
    )
}
