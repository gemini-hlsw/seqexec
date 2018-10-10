// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import monocle.Getter
import monocle.Optional
import monocle.Traversal
import monocle.macros.Lenses
import monocle.std
import monocle.function.At.at
import seqexec.model.ExecutionQueueView
import seqexec.model.QueueId
import seqexec.model.enum.BatchCommandState
import seqexec.web.client.model._

@Lenses
final case class CalQueueControlFocus(canOperate: Boolean,
                                      state:      BatchCommandState,
                                      ops:        QueueOperations)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalQueueControlFocus {
  implicit val eq: Eq[CalQueueControlFocus] =
    Eq.by(x => (x.canOperate, x.state, x.ops))

  def optQueue(id: QueueId): Optional[SeqexecAppRootModel, QueueOperations] =
    SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.queues     ^|->
      CalibrationQueues.queues  ^|->
      at(id)                    ^<-?
      std.option.some           ^|->
      CalQueueState.ops

  def queueState(id: QueueId): Traversal[SeqexecAppRootModel, BatchCommandState] =
    SeqexecAppRootModel.executionQueuesT(id) ^|->
      ExecutionQueueView.cmdState

  def queueControlG(id: QueueId): Getter[SeqexecAppRootModel, Option[CalQueueControlFocus]] =
    ClientStatus.canOperateG.zip(
      Getter(optQueue(id).getOption)
        .zip(Getter(queueState(id).headOption))) >>> {
      case (status, (Some(c), Some(s))) =>
        CalQueueControlFocus(status, s, c).some
      case _ =>
        none
    }
}
