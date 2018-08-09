// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import monocle.macros.Lenses

import cats._
import cats.implicits._
import gem.Observation
import seqexec.model.enum._

// @SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf"))
// scalastyle:off
object Model {

  type ParamName = String
  type ParamValue = String
  type Parameters = Map[ParamName, ParamValue]
  type StepConfig = Map[SystemName, Parameters]
  implicit val stEq: Eq[StepConfig] = Eq.fromUniversalEquals
  type StepId = Int
  type ObservationName = String
  type TargetName = String
  type ClientID = java.util.UUID
  implicit val clientIdEq: Eq[ClientID] = Eq.fromUniversalEquals
  val DaytimeCalibrationTargetName = "Daytime calibration"




  /**
    * Metadata about the sequence required on the exit point
    */
  // TODO Use a proper instrument class
  @Lenses final case class SequenceMetadata(
    instrument: Instrument,
    observer: Option[Observer],
    name: String
  )

@SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf"))
  object SequenceMetadata {
    implicit val eq: Eq[SequenceMetadata] =
      Eq.by(x => (x.instrument, x.observer, x.name))
  }

  @Lenses final case class SequenceView (
    id: Observation.Id,
    metadata: SequenceMetadata,
    status: SequenceState,
    steps: List[Step],
    willStopIn: Option[Int]
  )

@SuppressWarnings(Array("org.wartremover.warts.PublicInference", "org.wartremover.warts.IsInstanceOf"))
  object SequenceView {
    implicit val eq: Eq[SequenceView] =
      Eq.by { x =>
        (x.id, x.metadata, x.status, x.steps, x.willStopIn)
      }
  }


}
//scalastyle:on
