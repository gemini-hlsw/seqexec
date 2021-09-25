// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import scala.collection.immutable.SortedMap

import cats.Eq
import monocle.Lens
import monocle.Optional
import monocle.Prism
import monocle.function.At.at
import monocle.function.At.atSortedMap
import monocle.macros.Lenses
import monocle.std.option.some
import seqexec.model.Observation
import seqexec.model.Progress
import seqexec.model.StepId

/**
 * UI record of the remaining time for observations
 */
@Lenses
final case class AllObservationsProgressState(
  obsProgress: SortedMap[(Observation.Id, StepId), Progress]
)

object AllObservationsProgressState {
  val Empty: AllObservationsProgressState           =
    AllObservationsProgressState(SortedMap.empty)

  implicit val eq: Eq[AllObservationsProgressState] =
    Eq.by(_.obsProgress)

  def progressStateO[P <: Progress](
    obsId:                  Observation.Id,
    stepId:                 StepId
  )(implicit progressPrism: Prism[Progress, P]): Optional[SeqexecAppRootModel, P] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.obsProgress ^|-?
      progressByIdO(obsId, stepId)

  def progressByIdL(
    obsId:  Observation.Id,
    stepId: StepId
  ): Lens[AllObservationsProgressState, Option[Progress]] =
    AllObservationsProgressState.obsProgress ^|-> at((obsId, stepId))

  def progressByIdO[P <: Progress](
    obsId:                  Observation.Id,
    stepId:                 StepId
  )(implicit progressPrism: Prism[Progress, P]): Optional[AllObservationsProgressState, P] =
    progressByIdL(obsId, stepId) ^<-? some ^<-? progressPrism

}
