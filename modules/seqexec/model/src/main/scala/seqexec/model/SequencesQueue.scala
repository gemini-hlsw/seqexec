// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._
import gem.Observation
import monocle.Getter
import monocle.macros.Lenses
import seqexec.model.enum.Instrument

/**
  * Represents a queue with different levels of details. E.g. it could be a list of Ids
  * Or a list of fully hydrated SequenceViews
  */
@Lenses
final case class SequencesQueue[T](
  loaded:       Map[Instrument, Observation.Id],
  conditions:   Conditions,
  operator:     Option[Operator],
  sessionQueue: List[T]
)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequencesQueue {

  implicit def equal[T: Eq]: Eq[SequencesQueue[T]] =
    Eq.by(x => (x.loaded, x.conditions, x.operator, x.sessionQueue))

  def queueItemG[T](pred: T => Boolean): Getter[SequencesQueue[T], Option[T]] =
    SequencesQueue.sessionQueue
      .composeGetter(Getter[List[T], Option[T]](_.find(pred)))
}
