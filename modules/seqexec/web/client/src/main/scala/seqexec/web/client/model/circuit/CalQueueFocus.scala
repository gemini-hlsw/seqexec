// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import monocle.Getter
import monocle.macros.Lenses
import monocle.std
import monocle.function.At.at
import monocle.function.At.atMap
import seqexec.model.QueueId
import seqexec.web.client.model._

@Lenses
final case class CalQueueFocus(canOperate: Boolean, ops: QueueOperations)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalQueueFocus {
  implicit val eq: Eq[CalQueueFocus] =
    Eq.by(x => (x.canOperate, x.ops))

  def queueControlG(id: QueueId): Getter[SeqexecAppRootModel, Option[CalQueueFocus]] = {
    val optQueue =
      SeqexecAppRootModel.uiModel ^|->
      SeqexecUIModel.queues       ^|->
      CalibrationQueues.ops       ^|->
      at(id)                      ^<-?
      std.option.some

    ClientStatus.canOperateG.zip(Getter(optQueue.getOption)) >>> {
      case (status, Some(c)) =>
        CalQueueFocus(status, c).some
      case _ =>
        none
    }
  }
}
