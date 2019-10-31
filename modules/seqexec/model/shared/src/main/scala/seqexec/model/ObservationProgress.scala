// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Prism
import monocle.macros.GenPrism
import squants.Time

sealed trait Progress extends Product with Serializable {
  val obsId:     Observation.Id
  val stepId:    StepId
  val total:     Time
  val remaining: Time
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
}

final case class ObservationProgress(obsId:     Observation.Id,
                                     stepId:    StepId,
                                     total:     Time,
                                     remaining: Time) extends Progress

object ObservationProgress {

  implicit val equalObservationProgress: Eq[ObservationProgress] =
    Eq.by(x => (x.obsId, x.stepId, x.total, x.remaining))

}

final case class NSObservationProgress(obsId:     Observation.Id,
                                       stepId:    StepId,
                                       total:     Time,
                                       remaining: Time,
                                       sub:       NSSubexposure) extends Progress

object NSObservationProgress {

  implicit val equalNSObservationProgress: Eq[NSObservationProgress] =
    Eq.by(x => (x.obsId, x.stepId, x.total, x.remaining, x.sub))

}
