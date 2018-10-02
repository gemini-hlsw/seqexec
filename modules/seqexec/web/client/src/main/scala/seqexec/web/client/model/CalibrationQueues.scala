// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.CalibrationQueueId
import seqexec.model.QueueId

@Lenses
final case class CalibrationQueues(ops: Map[QueueId, QueueOperations])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalibrationQueues {
  implicit val eq: Eq[CalibrationQueues] =
    Eq.by(x => (x.ops))

  val Default: CalibrationQueues =
    CalibrationQueues(Map(CalibrationQueueId -> QueueOperations.Default))
}
