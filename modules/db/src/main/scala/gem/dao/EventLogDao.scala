package gem.dao

import gem.{Event, Sequence}
import gem.Event._
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

//  private def ts(i: Instant): Timestamp =
//    Timestamp.from(i)

  def selectAll(start: Instant, end: Instant): ConnectionIO[List[Event]] =
    sql"""
      SELECT timestamp,
             event :: evt_type,
             sequence_id,
             step
        FROM log_observe_event
       WHERE timestamp BETWEEN $start AND $end
    ORDER BY timestamp
    """.query[(Instant, EventType, Sequence.Id, Option[Int])].map {
      case (t, Abort,            s, None   ) => abortObserve(t, s)
      case (t, Continue,         s, None   ) => continueObserve(t, s)
      case (t, EndIntegration,   s, Some(i)) => endIntegration(t, s, i)
      case (t, EndSequence,      s, None   ) => endSequence(t, s)
      case (t, EndSlew,          s, None   ) => endSlew(t, s)
      case (t, EndVisit,         s, None   ) => endVisit(t, s)
      case (t, Pause,            s, None   ) => pauseObserve(t, s)
      case (t, StartIntegration, s, Some(i)) => startIntegration(t, s, i)
      case (t, StartSequence,    s, None   ) => startSequence(t, s)
      case (t, StartSlew,        s, None   ) => startSlew(t, s)
      case (t, StartVisit,       s, None   ) => startVisit(t, s)
      case (t, Stop,             s, None   ) => stopObserve(t, s)
      case (t, e,                s, step   ) =>
        sys.error(s"Unexpected row in log_observe_event: ($t, $e, $s, $step)")
    }.list

}
