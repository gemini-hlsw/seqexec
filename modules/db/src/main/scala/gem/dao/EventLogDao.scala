// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.dao

import doobie._, doobie.implicits._
import gem.{Event, Observation}
import gem.dao.meta._
import gem.Event._
import gem.enum.EventType
import gem.enum.EventType.{Abort, Continue, EndIntegration, EndSequence, EndSlew, EndVisit, Pause, StartIntegration, StartSequence, StartSlew, StartVisit, Stop}
import java.time.Instant


object EventLogDao {
  import EnumeratedMeta._
  import ObservationIdMeta._

  def insertAbortObserve(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Abort, oid, None).run

  def insertContinue(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Continue, oid, None).run

  def insertEndIntegration(oid: Observation.Id, step: Int): ConnectionIO[Int] =
    Statements.insertEvent(EndIntegration, oid, Some(step)).run

  def insertEndSequence(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(EndSequence, oid, None).run

  def insertEndSlew(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(EndSlew, oid, None).run

  def insertEndVisit(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(EndVisit, oid, None).run

  def insertPauseObserve(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Pause, oid, None).run

  def insertStartIntegration(oid: Observation.Id, step: Int): ConnectionIO[Int] =
    Statements.insertEvent(StartIntegration, oid, Some(step)).run

  def insertStartSequence(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(StartSequence, oid, None).run

  def insertStartSlew(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(StartSlew, oid, None).run

  def insertStartVisit(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(StartVisit, oid, None).run

  def insertStop(oid: Observation.Id): ConnectionIO[Int] =
    Statements.insertEvent(Stop, oid, None).run

  def selectAll(start: Instant, end: Instant): ConnectionIO[List[Event]] =
    Statements.selectAll(start, end).to[List]

  object Statements {

    def insertEvent(t: EventType, oid: Observation.Id, step: Option[Int]): Update0 =
      sql"""
        INSERT INTO log_observe_event (event, observation_id, step)
             VALUES ($t :: evt_type,
                     $oid,
                     $step)
      """.update

    def selectAll(start: Instant, end: Instant): Query0[Event] =
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
      }

  }

}
