// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import java.time.Instant

/**
 * An observing event, with many constuctors.
 * @group Sequence Model
 */
sealed trait Event {
  def timestamp: Instant
  def oid: Observation.Id
}

object Event {

  final case class StartSlew(       timestamp: Instant, oid: Observation.Id) extends Event
  final case class EndSlew(         timestamp: Instant, oid: Observation.Id) extends Event

  final case class StartVisit(      timestamp: Instant, oid: Observation.Id) extends Event
  final case class EndVisit(        timestamp: Instant, oid: Observation.Id) extends Event

  final case class StartSequence(   timestamp: Instant, oid: Observation.Id) extends Event
  final case class EndSequence(     timestamp: Instant, oid: Observation.Id) extends Event

  final case class PauseObserve(    timestamp: Instant, oid: Observation.Id) extends Event
  final case class ContinueObserve( timestamp: Instant, oid: Observation.Id) extends Event
  final case class AbortObserve(    timestamp: Instant, oid: Observation.Id) extends Event
  final case class StopObserve(     timestamp: Instant, oid: Observation.Id) extends Event

  final case class StartIntegration(timestamp: Instant, oid: Observation.Id, step: Int) extends Event
  final case class EndIntegration(  timestamp: Instant, oid: Observation.Id, step: Int) extends Event


  def startSlew(t: Instant, o: Observation.Id): Event       = StartSlew(t, o)
  def endSlew(t: Instant, o: Observation.Id): Event         = EndSlew(t, o)

  def startVisit(t: Instant, o: Observation.Id): Event      = StartVisit(t, o)
  def endVisit(t: Instant, o: Observation.Id): Event        = EndVisit(t, o)

  def startSequence(t: Instant, o: Observation.Id): Event   = StartSequence(t, o)
  def endSequence(t: Instant, o: Observation.Id): Event     = EndSequence(t, o)

  def pauseObserve(t: Instant, o: Observation.Id): Event    = PauseObserve(t, o)
  def continueObserve(t: Instant, o: Observation.Id): Event = ContinueObserve(t, o)
  def abortObserve(t: Instant, o: Observation.Id): Event    = AbortObserve(t, o)
  def stopObserve(t: Instant, o: Observation.Id): Event     = StopObserve(t, o)

  def startIntegration(t: Instant, o: Observation.Id, i: Int): Event = StartIntegration(t, o, i)
  def endIntegration(t: Instant, o: Observation.Id, i: Int): Event   = EndIntegration(t, o, i)

}
