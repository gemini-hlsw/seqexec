// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import scala.collection.immutable.SortedMap

import cats.Eq
import monocle.Lens
import monocle.macros.Lenses
import seqexec.model.BatchCommandState
import seqexec.model.ExecutionQueueView
import seqexec.model.Observer
import seqexec.model.QueueId
import seqexec.model.SequenceView
import seqexec.model.SequencesQueue
import seqexec.web.client.model.SeqexecAppRootModel
import seqexec.web.client.model.SessionQueueFilter
import seqexec.web.client.model.ClientStatus

@Lenses
final case class QueueRequestsFocus(
  clientStatus: ClientStatus,
  sequences:    SequencesQueue[SequenceView],
  seqFilter:    SessionQueueFilter
)

object QueueRequestsFocus {
  implicit val eq: Eq[QueueRequestsFocus] =
    Eq.by(x => (x.clientStatus, x.sequences, x.seqFilter))

  def observers(m: SeqexecAppRootModel): SortedMap[QueueId, Observer] =
    SortedMap(SeqexecAppRootModel.queuesT.getAll(m).collect {
      case ExecutionQueueView(id, _, BatchCommandState.Run(o, _, _), _, _) =>
        (id, o)
    }: _*)

  val unsafeQueueRequestsFocusL: Lens[SeqexecAppRootModel, QueueRequestsFocus] =
    Lens[SeqexecAppRootModel, QueueRequestsFocus] { m =>
      val clLens = ClientStatus.clientStatusFocusL
      QueueRequestsFocus(clLens.get(m), m.sequences, SeqexecAppRootModel.sessionQueueFilterL.get(m))
    }(v => m => m.copy(clientId = v.clientStatus.clientId, sequences = v.sequences))

}
