// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._
import gem.Observation
import monocle.{Getter, Traversal}
import monocle.function.Each._
import monocle.macros.Lenses

import scala.collection.immutable.SortedMap
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
  queues:       SortedMap[QueueId, ExecutionQueueView],
  sessionQueue: List[T]
)

object SequencesQueue {

  implicit def equal[T: Eq]: Eq[SequencesQueue[T]] =
    Eq.by(x => (x.loaded, x.conditions, x.operator, x.queues, x.sessionQueue))

  def sessionQueueT[T]: Traversal[SequencesQueue[T], T] =
    SequencesQueue.sessionQueue[T] ^|->> each

  def queueItemG[T](pred: T => Boolean): Getter[SequencesQueue[T], Option[T]] =
    SequencesQueue.sessionQueue
      .composeGetter(Getter[List[T], Option[T]](_.find(pred)))
}
