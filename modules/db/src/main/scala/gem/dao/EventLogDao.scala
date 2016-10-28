package gem.dao

import gem.{Event, Observation}
import gem.Event._
import gem.enum.EventType
import gem.enum.EventType.{Abort, Continue, EndIntegration, EndSequence, EndSlew, EndVisit, Pause, StartIntegration, StartSequence, StartSlew, StartVisit, Stop}

import java.time.Instant
import doobie.imports._

import scalaz._, Scalaz._


object EventLogDao {

  private def insertEvent(t: EventType, oid: Observation.Id, step: Option[Int]): ConnectionIO[Int] =
    sql"""
      INSERT INTO log_observe_event (event, observation_id, step)
           VALUES ($t :: evt_type,
                   $oid,
                   $step)
    """.update.run

  def insertAbortObserve(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(Abort, oid, None)

  def insertContinue(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(Continue, oid, None)

  def insertEndIntegration(oid: Observation.Id, step: Int): ConnectionIO[Int] =
    insertEvent(EndIntegration, oid, Some(step))

  def insertEndSequence(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(EndSequence, oid, None)

  def insertEndSlew(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(EndSlew, oid, None)

  def insertEndVisit(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(EndVisit, oid, None)

  def insertPauseObserve(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(Pause, oid, None)

  def insertStartIntegration(oid: Observation.Id, step: Int): ConnectionIO[Int] =
    insertEvent(StartIntegration, oid, Some(step))

  def insertStartSequence(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(StartSequence, oid, None)

  def insertStartSlew(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(StartSlew, oid, None)

  def insertStartVisit(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(StartVisit, oid, None)

  def insertStop(oid: Observation.Id): ConnectionIO[Int] =
    insertEvent(Stop, oid, None)

  def selectAll(start: Instant, end: Instant): ConnectionIO[List[Event]] =
    sql"""
      SELECT timestamp,
             event :: evt_type,
             observation_id,
             step
        FROM log_observe_event
       WHERE timestamp BETWEEN $start AND $end
    ORDER BY timestamp
    """.query[(Instant, EventType, Observation.Id, Option[Int])].map {
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
