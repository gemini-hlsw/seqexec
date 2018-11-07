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
import scala.collection.immutable.SortedMap

/**
  * UI record of the remaining time for observations
  */
@Lenses
final case class ObservationsProgress(
  obsProgress: SortedMap[Observation.Id, ObservationProgress])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object ObservationsProgress {
  val Empty: ObservationsProgress = ObservationsProgress(SortedMap.empty)

  implicit val eq: Eq[ObservationsProgress] =
    Eq.by(_.obsProgress)

  def progressByIdL(
    obsId: Observation.Id
  ): Lens[ObservationsProgress, Option[ObservationProgress]] =
    ObservationsProgress.obsProgress ^|-> at(obsId)
}
