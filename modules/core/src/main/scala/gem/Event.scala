package gem

import java.time.Instant

sealed trait Event {
  def timestamp: Instant
  def sid: Sequence.Id
}

object Event {

  case class StartSlew(       timestamp: Instant, sid: Sequence.Id) extends Event
  case class EndSlew(         timestamp: Instant, sid: Sequence.Id) extends Event

  case class StartVisit(      timestamp: Instant, sid: Sequence.Id) extends Event
  case class EndVisit(        timestamp: Instant, sid: Sequence.Id) extends Event

  case class StartSequence(   timestamp: Instant, sid: Sequence.Id) extends Event
  case class EndSequence(     timestamp: Instant, sid: Sequence.Id) extends Event

  case class PauseObserve(    timestamp: Instant, sid: Sequence.Id) extends Event
  case class ContinueObserve( timestamp: Instant, sid: Sequence.Id) extends Event
  case class AbortObserve(    timestamp: Instant, sid: Sequence.Id) extends Event
  case class StopObserve(     timestamp: Instant, sid: Sequence.Id) extends Event

  case class StartIntegration(timestamp: Instant, sid: Sequence.Id, step: Int) extends Event
  case class EndIntegration(  timestamp: Instant, sid: Sequence.Id, step: Int) extends Event


  def startSlew(t: Instant, s: Sequence.Id): Event       = StartSlew(t, s)
  def endSlew(t: Instant, s: Sequence.Id): Event         = EndSlew(t, s)

  def startVisit(t: Instant, s: Sequence.Id): Event      = StartVisit(t, s)
  def endVisit(t: Instant, s: Sequence.Id): Event        = EndVisit(t, s)

  def startSequence(t: Instant, s: Sequence.Id): Event   = StartSequence(t, s)
  def endSequence(t: Instant, s: Sequence.Id): Event     = EndSequence(t, s)

  def pauseObserve(t: Instant, s: Sequence.Id): Event    = PauseObserve(t, s)
  def continueObserve(t: Instant, s: Sequence.Id): Event = ContinueObserve(t, s)
  def abortObserve(t: Instant, s: Sequence.Id): Event    = AbortObserve(t, s)
  def stopObserve(t: Instant, s: Sequence.Id): Event     = StopObserve(t, s)

  def startIntegration(t: Instant, s: Sequence.Id, i: Int): Event = StartIntegration(t, s, i)
  def endIntegration(t: Instant, s: Sequence.Id, i: Int): Event   = EndIntegration(t, s, i)

}
