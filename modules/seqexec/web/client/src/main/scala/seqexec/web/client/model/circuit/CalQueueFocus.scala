// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import monocle.Lens
import monocle.Traversal
import monocle.macros.Lenses
import monocle.function.Each.each
import monocle.function.Each.listEach
import seqexec.model.{ExecutionQueueView, QueueId}
import seqexec.web.client.model._
import seqexec.web.client.components.queue.CalQueueTable
import web.client.table.TableState

@Lenses
final case class CalQueueFocus(
  canOperate: Boolean,
  loggedIn:   Boolean,
  seqs:       List[CalQueueSeq],
  tableState: TableState[CalQueueTable.TableColumn])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalQueueFocus {
  implicit val eq: Eq[CalQueueFocus] =
    Eq.by(x => (x.canOperate, x.seqs, x.tableState))

  // A fairly complicated getter
  def calQueueG(id: QueueId): Getter[SeqexecAppRootModel, Option[CalQueueFocus]] = {
    // All ids on the queue
    val ids: Traversal[SeqexecAppRootModel, Observation.Id] =
      SeqexecAppRootModel.executionQueuesT(id) ^|->
        ExecutionQueueView.queue               ^|->>
        each

    // All metadata of the given obs
    def calSeq(id: Observation.Id): Getter[SeqexecAppRootModel, Option[CalQueueSeq]] =
      SeqexecAppRootModel.sequences.composeGetter(CalQueueSeq.calQueueSeqG(id))

    def calTS(id: QueueId): Lens[SeqexecAppRootModel, Option[TableState[CalQueueTable.TableColumn]]] =
      SeqexecAppRootModel.uiModel  ^|->
        AppTableStates.tableStateL ^|->
        AppTableStates.queueTableAtL(id)

    // combine
    val calQueueSeqG = (s: SeqexecAppRootModel) =>
      ids.getAll(s).map(i => calSeq(i).get(s))

    ClientStatus.clientStatusFocusL.asGetter
      .zip(Getter(calQueueSeqG).zip(calTS(id).asGetter)) >>> {
      case (status, (ids, ts)) =>
        CalQueueFocus(status.canOperate, status.isLogged, ids.collect {
          case Some(x) => x
        }, ts.getOrElse(CalQueueTable.State.ROTableState)).some
      case _ =>
        none
    }
  }
}
