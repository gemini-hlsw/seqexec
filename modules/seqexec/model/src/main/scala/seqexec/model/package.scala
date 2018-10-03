// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec

import cats._
import java.util.UUID
import seqexec.model.enum._

package object model {
  type ParamName       = String
  type ParamValue      = String
  type Parameters      = Map[ParamName, ParamValue]
  type StepConfig      = Map[SystemName, Parameters]
  type StepId          = Int
  type ObservationName = String
  type TargetName      = String
  type ClientID        = UUID
  type QueueId         = UUID

  implicit val stEq: Eq[StepConfig]     = Eq.fromUniversalEquals
  implicit val clientIdEq: Eq[ClientID] = Eq.fromUniversalEquals
  val DaytimeCalibrationTargetName      = "Daytime calibration"

  val CalibrationQueueName: String = "Calibration Queue"
  val CalibrationQueueId: ClientID = UUID.fromString("7156fa7e-48a6-49d1-a267-dbf3bbaa7577")

}
