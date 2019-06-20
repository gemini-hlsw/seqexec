// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import monocle.Lens
import monocle.Optional
import monocle.macros.Lenses
import scala.collection.immutable.SortedMap
import seqexec.model.ClientId
import seqexec.model.QueueId
import seqexec.model.Observer
import seqexec.model.ExecutionQueueView
import seqexec.model.BatchCommandState
import seqexec.model.SequencesQueue
import seqexec.model.SequenceView
import seqexec.web.client.model.SeqexecAppRootModel
import seqexec.web.client.model.SeqexecUIModel
import seqexec.web.client.model.SequencesOnDisplay
import seqexec.web.client.model.SessionQueueFilter

@Lenses
final case class QueueRequestsFocus(
  clientId:       Option[ClientId],
  sequences:      SequencesQueue[SequenceView],
  calTabObserver: Option[Observer],
  queuesObserver: SortedMap[QueueId, Observer],
  seqFilter:      SessionQueueFilter)

object QueueRequestsFocus {
  implicit val eq: Eq[QueueRequestsFocus] =
    Eq.by(x => (x.clientId, x.sequences, x.calTabObserver, x.queuesObserver))

  def observers(m: SeqexecAppRootModel): SortedMap[QueueId, Observer] =
    SortedMap(SeqexecAppRootModel.queuesT.getAll(m).collect {
      case ExecutionQueueView(id, _, BatchCommandState.Run(o, _, _), _, _) =>
        (id, o)
    }: _*)

  val calTabObserverL: Optional[SeqexecAppRootModel, Observer] =
    SeqexecAppRootModel.uiModel         ^|->
      SeqexecUIModel.sequencesOnDisplay ^|-?
      SequencesOnDisplay.calTabObserver

  // This lens is read only but a getter is not usable in diode
  val unsafeQueueRequestsFocusL: Lens[SeqexecAppRootModel, QueueRequestsFocus] =
    Lens[SeqexecAppRootModel, QueueRequestsFocus](
      m =>
        QueueRequestsFocus(m.clientId,
                           m.sequences,
                           calTabObserverL.getOption(m),
                           observers(m),
                           SeqexecAppRootModel.sessionQueueFilterL.get(m)))(v =>
      m => m.copy(clientId = v.clientId, sequences = v.sequences))

}
