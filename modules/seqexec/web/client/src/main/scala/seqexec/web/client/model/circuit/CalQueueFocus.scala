// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import monocle.Traversal
import monocle.macros.Lenses
import monocle.function.Each.each
import monocle.function.Each.listEach
import monocle.function.FilterIndex.filterIndex
import monocle.unsafe.MapTraversal._
import seqexec.model.ExecutionQueue
import seqexec.model.QueueId
import seqexec.model.SequencesQueue
import seqexec.web.client.model._

@Lenses
final case class CalQueueFocus(canOperate: Boolean, seqs: List[CalQueueSeq])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalQueueFocus {
  implicit val eq: Eq[CalQueueFocus] =
    Eq.by(x => (x.canOperate, x.seqs))

  // A fairly complicated getter
  def calQueueG(id: QueueId): Getter[SeqexecAppRootModel, Option[CalQueueFocus]] = {
    // All ids on the queue
    val ids: Traversal[SeqexecAppRootModel, Observation.Id] =
      SeqexecAppRootModel.sequences               ^|->
        SequencesQueue.queues                     ^|->>
        filterIndex((qid: QueueId) => qid === id) ^|->
        ExecutionQueue.queue ^|->>
        each

    // All metadata of the given obs
    def calSeq(id: Observation.Id): Getter[SeqexecAppRootModel, Option[CalQueueSeq]] =
      SeqexecAppRootModel.sequences.composeGetter(CalQueueSeq.calQueueSeqG(id))

    // combine
    val calQueueSeqG = (s: SeqexecAppRootModel) =>
      ids.getAll(s).map(i => calSeq(i).get(s))

    ClientStatus.canOperateG.zip(Getter(calQueueSeqG)) >>> {
      case (status, ids) =>
        CalQueueFocus(status, ids.collect { case Some(x) => x }).some
      case _ =>
        none
    }
  }
}
