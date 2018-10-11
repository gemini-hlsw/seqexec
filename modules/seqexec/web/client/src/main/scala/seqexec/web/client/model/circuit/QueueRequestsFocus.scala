// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
// import monocle.Getter
import monocle.Lens
// import monocle.Traversal
import monocle.macros.Lenses
// import monocle.function.Each.each
// import monocle.function.Each.listEach
// import monocle.function.FilterIndex.filtersIndex
import scala.collection.immutable.SortedMap
import seqexec.model.ClientId
import seqexec.model.QueueId
import seqexec.model.Observer
import seqexec.model.ExecutionQueueView
import seqexec.model.enum.BatchCommandState
import seqexec.model.SequencesQueue
import seqexec.model.SequenceView
import seqexec.web.client.model.SeqexecAppRootModel
import seqexec.web.client.model.SeqexecUIModel

@Lenses
final case class QueueRequestsFocus(
  clientId:        Option[ClientId],
  sequences:       SequencesQueue[SequenceView],
  defaultObserver: Observer,
  queuesObserver:  SortedMap[QueueId, Observer])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object QueueRequestsFocus {
  implicit val eq: Eq[QueueRequestsFocus] =
    Eq.by(x => (x.clientId, x.sequences, x.defaultObserver, x.queuesObserver))

  def observers(m: SeqexecAppRootModel): SortedMap[QueueId, Observer] =
    SortedMap(SeqexecAppRootModel.queuesT.getAll(m).collect {
      case ExecutionQueueView(id, _, BatchCommandState.Run(o, _, _), _, _) =>
        (id, o)
    }: _*)

  val defaultObserverL: Lens[SeqexecAppRootModel, Observer] =
    SeqexecAppRootModel.uiModel ^|-> SeqexecUIModel.defaultObserver

  // This lens is read only but a getter is not usable in diode
  val unsafeQueueRequestsFocusL: Lens[SeqexecAppRootModel, QueueRequestsFocus] =
    Lens[SeqexecAppRootModel, QueueRequestsFocus](
      m =>
        QueueRequestsFocus(m.clientId,
                           m.sequences,
                           defaultObserverL.get(m),
                           observers(m)))(v =>
      m => m.copy(clientId = v.clientId, sequences = v.sequences))

}
