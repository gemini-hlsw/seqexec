// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import gem.enum.Site
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model._
import seqexec.model.enum.Resource
import seqexec.web.client.model.{AlignAndCalibStep, Pages, ResourceRunOperation, SeqexecAppRootModel, SequenceTab, SequencesOnDisplay, SoundSelection}

import scala.collection.immutable.SortedMap

@Lenses
final case class WebSocketsFocus(location:             Pages.SeqexecPages,
                                 sequences:            SequencesQueue[SequenceView],
                                 resourceRunRequested: Map[Observation.Id, SortedMap[Resource, ResourceRunOperation]],
                                 user:                 Option[UserDetails],
                                 defaultObserver:      Observer,
                                 clientId:             Option[ClientId],
                                 site:                 Option[Site],
                                 sound:                SoundSelection,
                                 serverVersion:        Option[String],
                                 guideConfig:          TelescopeGuideConfig,
                                 alignAndCalib:        AlignAndCalibStep)

object WebSocketsFocus {
  implicit val eq: Eq[WebSocketsFocus] =
    Eq.by(
      x =>
        (x.location,
         x.sequences,
         x.user,
         x.defaultObserver,
         x.clientId,
         x.site,
         x.serverVersion,
         x.guideConfig,
         x.alignAndCalib))

  val webSocketFocusL: Lens[SeqexecAppRootModel, WebSocketsFocus] =
    Lens[SeqexecAppRootModel, WebSocketsFocus](
      m =>
        WebSocketsFocus(m.uiModel.navLocation,
                        m.sequences,
                        SeqexecAppRootModel.sequenceTabsT.getAll(m)
                          .map(t => t.obsId -> t.tabOperations.resourceRunRequested).toMap,
                        m.uiModel.user,
                        m.uiModel.defaultObserver,
                        m.clientId,
                        m.site,
                        m.uiModel.sound,
                        m.serverVersion,
                        m.guideConfig,
                        m.alignAndCalib))(
      v =>
        m =>
          m.copy(
            sequences = v.sequences,
            uiModel = m.uiModel.copy(
              user = v.user,
              sequencesOnDisplay =
                SequencesOnDisplay.sequenceTabs.modify(
                  seqTab => SequenceTab.resourcesRunOperationsL.set(v.resourceRunRequested
                                            .getOrElse(seqTab.obsId, SortedMap.empty))(seqTab)
                  )(m.uiModel.sequencesOnDisplay),
              defaultObserver = v.defaultObserver,
              sound           = v.sound),
            clientId      = v.clientId,
            site          = v.site,
            serverVersion = v.serverVersion,
            guideConfig   = v.guideConfig,
            alignAndCalib = v.alignAndCalib
      ))
}
