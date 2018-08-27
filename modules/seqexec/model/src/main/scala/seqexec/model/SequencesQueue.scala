// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._
import gem.Observation
import seqexec.model.enum.Instrument

/**
  * Represents a queue with different levels of details. E.g. it could be a list of Ids
  * Or a list of fully hydrated SequenceViews
  */
final case class SequencesQueue[T](
  loaded:     Map[Instrument, Observation.Id],
  conditions: Conditions,
  operator:   Option[Operator],
  queue:      List[T]
)

object SequencesQueue {

  implicit def equal[T: Eq]: Eq[SequencesQueue[T]] =
    Eq.by(x => (x.conditions, x.operator, x.queue))

}
