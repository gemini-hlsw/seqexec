package gem

import java.time.Instant

object EventLog {
  sealed trait Event {
    def timestamp: Instant
    def sid: Sequence.Id
  }

  case class EventStartSlew(    timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventEndSlew(      timestamp: Instant, sid: Sequence.Id) extends Event

  case class EventStartVisit(   timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventEndVisit(     timestamp: Instant, sid: Sequence.Id) extends Event

  case class EventStartSequence(timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventEndSequence(  timestamp: Instant, sid: Sequence.Id) extends Event

  case class EventPauseObserve(   timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventContinueObserve(timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventAbortObserve(   timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventStopObserve(    timestamp: Instant, sid: Sequence.Id) extends Event

  case class EventStartIntegration(timestamp: Instant, sid: Sequence.Id, step: Int) extends Event
  case class EventEndIntegration(  timestamp: Instant, sid: Sequence.Id, step: Int) extends Event
}
