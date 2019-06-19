// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import monocle.macros.Lenses
import monocle.std
import seqexec.model.enum.Instrument
import seqexec.model.SequencesQueue
import seqexec.model.SequenceView
import seqexec.model.SequenceMetadata
import seqexec.model.SequenceState

@Lenses
final case class CalQueueSeq(id:     Observation.Id,
                             i:      Instrument,
                             status: SequenceState)

object CalQueueSeq {
  implicit val eq: Eq[CalQueueSeq] =
    Eq.by(x => (x.id, x.id, x.status))

  def calQueueSeqG(id: Observation.Id): Getter[SequencesQueue[SequenceView], Option[CalQueueSeq]] = {
    val seqO =
      SequencesQueue.queueItemG[SequenceView](_.id === id) ^<-?
        std.option.some

    val sidO = seqO ^|-> SequenceView.id
    val siO  = seqO ^|-> SequenceView.metadata ^|-> SequenceMetadata.instrument
    val siS  = seqO ^|-> SequenceView.status

    (Getter(sidO.headOption)
      .zip(Getter(siO.headOption).zip(Getter(siS.headOption)))) >>> {
      case (Some(id), (Some(i), Some(s))) => CalQueueSeq(id, i, s).some
      case _                              => none
    }
  }
}
