// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
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

@Lenses
final case class CalQueueSeq(id: Observation.Id, i: Instrument)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalQueueSeq {
  implicit val eq: Eq[CalQueueSeq] =
    Eq.by(x => (x.id, x.id))

  def calQueueSeqG(id: Observation.Id): Getter[SequencesQueue[SequenceView], Option[CalQueueSeq]] = {
    val seqO =
      SequencesQueue.queueItemG[SequenceView](_.id === id) ^<-?
        std.option.some

    val sidO = seqO ^|-> SequenceView.id
    val siO  = seqO ^|-> SequenceView.metadata ^|-> SequenceMetadata.instrument

    (Getter(sidO.headOption).zip(Getter(siO.headOption))) >>> {
      _.mapN(CalQueueSeq.apply)
    }
  }
}
