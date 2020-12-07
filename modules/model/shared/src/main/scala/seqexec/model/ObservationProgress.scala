// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.util.Enumerated
import monocle.Iso
import monocle.Prism
import monocle.macros.GenPrism
import seqexec.model.Observation
import squants.Time

sealed trait Progress extends Product with Serializable {
  val obsId:     Observation.Id
  val stepId:    StepId
  val total:     Time
  val remaining: Time
  val stage:     ObserveStage
}

object Progress {

  implicit val equalProgress: Eq[Progress] =
    Eq.instance {
      case (a: ObservationProgress, b: ObservationProgress)     => a === b
      case (a: NSObservationProgress, b: NSObservationProgress) => a === b
      case _                                                    => false
    }

  implicit val obsProgressP: Prism[Progress, ObservationProgress] =
    GenPrism[Progress, ObservationProgress]

  implicit val nsProgressP: Prism[Progress, NSObservationProgress] =
    GenPrism[Progress, NSObservationProgress]

  implicit val progressP: Prism[Progress, Progress] =
    Iso.id[Progress].asPrism
}

final case class ObservationProgress(obsId:     Observation.Id,
                                     stepId:    StepId,
                                     total:     Time,
                                     remaining: Time,
                                     stage:     ObserveStage) extends Progress

object ObservationProgress {

  implicit val equalObservationProgress: Eq[ObservationProgress] =
    Eq.by(x => (x.obsId, x.stepId, x.total, x.remaining, x.stage))

}

final case class NSObservationProgress(obsId:     Observation.Id,
                                       stepId:    StepId,
                                       total:     Time,
                                       remaining: Time,
                                       stage:     ObserveStage,
                                       sub:       NSSubexposure) extends Progress

object NSObservationProgress {

  implicit val equalNSObservationProgress: Eq[NSObservationProgress] =
    Eq.by(x => (x.obsId, x.stepId, x.total, x.remaining, x.stage, x.sub))

}

sealed trait ObserveStage extends Product with Serializable

object ObserveStage {

  case object Idle extends ObserveStage
  case object Preparing extends ObserveStage
  case object Acquiring extends ObserveStage
  case object ReadingOut extends ObserveStage

  implicit val observeStageEnum: Enumerated[ObserveStage] = Enumerated.of(Idle, Preparing, Acquiring, ReadingOut)

  def fromBooleans(prep: Boolean, acq: Boolean, rdout: Boolean): ObserveStage =
    if(prep) Preparing
    else if(acq) Acquiring
    else if(rdout) ReadingOut
    else Idle

}
