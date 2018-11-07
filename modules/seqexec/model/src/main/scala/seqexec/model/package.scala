// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec

import cats._
import cats.implicits._
import java.util.UUID
import squants.Time
import squants.time.TimeUnit
import seqexec.model.enum._

package model {
  final case class QueueId(self:  UUID) extends AnyVal
  final case class ClientId(self: UUID) extends AnyVal
}

package object model {
  type ParamName       = String
  type ParamValue      = String
  type Parameters      = Map[ParamName, ParamValue]
  type StepConfig      = Map[SystemName, Parameters]
  type StepId          = Int
  type ObservationName = String
  type TargetName      = String

  implicit val queueIdEq: Eq[QueueId]     = Eq.by(x => x.self)
  implicit val queueIdShow: Show[QueueId] = Show.fromToString
  implicit val queueIdOrder: Order[QueueId] =
    Order.by(_.self)
  implicit val queueIdOrdering: scala.math.Ordering[QueueId] =
    queueIdOrder.toOrdering

  implicit val stEq: Eq[StepConfig]         = Eq.fromUniversalEquals
  implicit val clientIdEq: Eq[ClientId]     = Eq.by(x => x.self)
  implicit val clientIdShow: Show[ClientId] = Show.fromToString
  implicit val clientIdOrder: Order[ClientId] =
    Order.by(_.self)
  implicit val clientIdOrdering: scala.math.Ordering[ClientId] =
    clientIdOrder.toOrdering
  val DaytimeCalibrationTargetName = "Daytime calibration"

  val CalibrationQueueName: String = "Calibration Queue"
  val CalibrationQueueId: QueueId =
    QueueId(UUID.fromString("7156fa7e-48a6-49d1-a267-dbf3bbaa7577"))

  implicit val timeUnit: Eq[TimeUnit] =
    Eq.by(_.symbol)
  implicit val timeEq: Eq[Time] =
    Eq.by(t => (t.value, t.unit))

}
