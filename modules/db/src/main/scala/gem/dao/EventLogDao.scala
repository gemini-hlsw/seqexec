package gem.dao

import gem.{EventLog, Sequence}
import gem.EventLog._
import gem.enum.EventType
import gem.enum.EventType.{Abort, Continue, EndIntegration, EndSequence, EndSlew, EndVisit, Pause, StartIntegration, StartSequence, StartSlew, StartVisit, Stop}

import java.sql.Timestamp
import java.time.Instant
import doobie.imports._

import scalaz._
import Scalaz._



object EventLogDao {

  private def insertEvent(t: EventType, sid: Sequence.Id, step: Option[Int]): ConnectionIO[Int] =
    sql"""
      INSERT INTO log_observe_event (event, sequence_id, step)
           VALUES ($t :: evt_type,
                   $sid,
                   $step)
    """.update.run

  def insertAbortObserve(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(Abort, sid, None)

  def insertContinue(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(Continue, sid, None)

  def insertEndIntegration(sid: Sequence.Id, step: Int): ConnectionIO[Int] =
    insertEvent(EndIntegration, sid, Some(step))

  def insertEndSequence(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(EndSequence, sid, None)

  def insertEndSlew(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(EndSlew, sid, None)

  def insertEndVisit(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(EndVisit, sid, None)

  def insertPauseObserve(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(Pause, sid, None)

  def insertStartIntegration(sid: Sequence.Id, step: Int): ConnectionIO[Int] =
    insertEvent(StartIntegration, sid, Some(step))

  def insertStartSequence(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(StartSequence, sid, None)

  def insertStartSlew(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(StartSlew, sid, None)

  def insertStartVisit(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(StartVisit, sid, None)

  def insertStop(sid: Sequence.Id): ConnectionIO[Int] =
    insertEvent(Stop, sid, None)

  private def ts(i: Instant): Timestamp =
    Timestamp.from(i)

  def selectAll(start: Instant, end: Instant): ConnectionIO[List[EventLog.Event]] =
    sql"""
      SELECT timestamp,
             event :: evt_type,
             sequence_id,
             step
        FROM log_observe_event
       WHERE timestamp BETWEEN ${ts(start)} AND ${ts(end)}
    ORDER BY timestamp
    """.query[(Instant, EventType, Sequence.Id, Option[Int])].map { case (t,e,s,i) =>

      (e match {
        case Abort            => EventAbortObserve(t, s)
        case Continue         => EventContinueObserve(t, s)
        case EndIntegration   => EventEndIntegration(t, s, i.get)
        case EndSequence      => EventEndSequence(t, s)
        case EndSlew          => EventEndSlew(t, s)
        case EndVisit         => EventEndVisit(t, s)
        case Pause            => EventPauseObserve(t, s)
        case StartIntegration => EventStartIntegration(t, s, i.get)
        case StartSequence    => EventStartSequence(t, s)
        case StartSlew        => EventStartSlew(t, s)
        case StartVisit       => EventStartVisit(t, s)
        case Stop             => EventStopObserve(t, s)
      }): EventLog.Event
    }.list

}
