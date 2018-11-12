// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Lens
import monocle.macros.Lenses
import monocle.function.At.at
import monocle.function.At.atSortedMap
import seqexec.model.ObservationProgress
import seqexec.model.StepId
import scala.collection.immutable.SortedMap

/**
  * UI record of the remaining time for observations
  */
@Lenses
final case class AllObservationsProgressState(
  obsProgress: SortedMap[(Observation.Id, StepId), ObservationProgress])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object AllObservationsProgressState {
  val Empty: AllObservationsProgressState =
    AllObservationsProgressState(SortedMap.empty)

  implicit val eq: Eq[AllObservationsProgressState] =
    Eq.by(_.obsProgress)

  def progressStateL(
    obsId:  Observation.Id,
    stepId: StepId
  ): Lens[SeqexecAppRootModel, Option[ObservationProgress]] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.obsProgress ^|->
      progressByIdL(obsId, stepId)

  def progressByIdL(
    obsId:  Observation.Id,
    stepId: StepId
  ): Lens[AllObservationsProgressState, Option[ObservationProgress]] =
    AllObservationsProgressState.obsProgress ^|-> at((obsId, stepId))

}
