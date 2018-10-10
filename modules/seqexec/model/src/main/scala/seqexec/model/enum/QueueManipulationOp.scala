// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq
import cats.implicits._
import gem.Observation
import seqexec.model.QueueId

sealed trait QueueManipulationOp extends Product with Serializable {
  val qid: QueueId
}

object QueueManipulationOp {
  final case class Moved(qid:     QueueId) extends QueueManipulationOp
  final case class Started(qid:   QueueId) extends QueueManipulationOp
  final case class Stopped(qid:   QueueId) extends QueueManipulationOp
  final case class Clear(qid:     QueueId) extends QueueManipulationOp
  final case class AddedSeqs(qid: QueueId, seqs: List[Observation.Id])
      extends QueueManipulationOp
  final case class RemovedSeqs(qid: QueueId, seqs: List[Observation.Id])
      extends QueueManipulationOp

  implicit val equal: Eq[QueueManipulationOp] = Eq.instance {
    case (Moved(a), Moved(b))                   => a === b
    case (Started(a), Started(b))               => a === b
    case (Stopped(a), Stopped(b))               => a === b
    case (Clear(a), Clear(b))                   => a === b
    case (AddedSeqs(a, c), AddedSeqs(b, d))     => a === b && c === d
    case (RemovedSeqs(a, c), RemovedSeqs(b, d)) => a === b && c === d
    case _                                      => false
  }
}
