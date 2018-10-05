// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq
import cats.implicits._
import seqexec.model.QueueId

sealed trait QueueManipulationOp extends Product with Serializable {
  val qid: QueueId
}

object QueueManipulationOp {
  final case class Modified(qid: QueueId) extends QueueManipulationOp
  final case class Started(qid: QueueId) extends QueueManipulationOp
  final case class Stopped(qid: QueueId) extends QueueManipulationOp

  implicit val equal: Eq[QueueManipulationOp] = Eq.by(_.qid)
}
