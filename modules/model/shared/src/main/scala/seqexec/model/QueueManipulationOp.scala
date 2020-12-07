// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._
import seqexec.model.Observation

sealed trait QueueManipulationOp extends Product with Serializable {
  val qid: QueueId
}

object QueueManipulationOp {
  final case class Moved(qid: QueueId,
                         cid: ClientId,
                         oid: Observation.Id,
                         pos: Int)
      extends QueueManipulationOp
  final case class Started(qid:   QueueId) extends QueueManipulationOp
  final case class Stopped(qid:   QueueId) extends QueueManipulationOp
  final case class Clear(qid:     QueueId) extends QueueManipulationOp
  final case class AddedSeqs(qid: QueueId, seqs: List[Observation.Id])
      extends QueueManipulationOp
  final case class RemovedSeqs(qid:       QueueId,
                               seqs:      List[Observation.Id],
                               positions: List[Int])
      extends QueueManipulationOp

  implicit val equal: Eq[QueueManipulationOp] = Eq.instance {
    case (Moved(a, c, e, g), Moved(b, d, f, h)) =>
      a === b && c === d && e === f && g === h
    case (Started(a), Started(b))           => a === b
    case (Stopped(a), Stopped(b))           => a === b
    case (Clear(a), Clear(b))               => a === b
    case (AddedSeqs(a, c), AddedSeqs(b, d)) => a === b && c === d
    case (RemovedSeqs(a, c, e), RemovedSeqs(b, d, f)) =>
      a === b && c === d && e === f
    case _ => false
  }
}
