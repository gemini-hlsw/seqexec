package gem.dao

import gem.{EventLog, Sequence}
import gem.enum.{EventLifecycle => Lifecycle, EventObserveCtrl => Ctrl, EventObserveStage => Stage}

import java.sql.Timestamp
import java.time.Instant
import doobie.imports._
import gem.EventLog._
import gem.enum.EventObserveStage.{Sequence => Seq, Slew, Visit}

import scalaz._
import Scalaz._



object EventLogDao {

  private def insertObserveStage(l: Lifecycle, s: Stage, sid: Sequence.Id): ConnectionIO[Int] =
    sql"""
      INSERT INTO log_observe_stage (lifecycle, observe_stage, sequence_id)
           VALUES ($l :: evt_lifecycle,
                   $s :: evt_observe_stage,
                   ${sid.toString})
    """.update.run

  def insertStartSequence(sid: Sequence.Id): ConnectionIO[Int] =
    insertObserveStage(Lifecycle.Start, Stage.Sequence, sid)

  def insertEndSequence(sid: Sequence.Id): ConnectionIO[Int] =
    insertObserveStage(Lifecycle.End, Stage.Sequence, sid)

  def insertStartSlew(sid: Sequence.Id): ConnectionIO[Int] =
    insertObserveStage(Lifecycle.Start, Stage.Slew, sid)

  def insertEndSlew(sid: Sequence.Id): ConnectionIO[Int] =
    insertObserveStage(Lifecycle.End, Stage.Slew, sid)

  def insertStartVisit(sid: Sequence.Id): ConnectionIO[Int] =
    insertObserveStage(Lifecycle.Start, Stage.Visit, sid)

  def insertEndVisit(sid: Sequence.Id): ConnectionIO[Int] =
    insertObserveStage(Lifecycle.End, Stage.Visit, sid)

  private def insertObserveCtrl(c: Ctrl, sid: Sequence.Id, why: Option[String]): ConnectionIO[Int] =
    sql"""
      INSERT INTO log_observe_ctrl (observe_ctrl, sequence_id, why)
           VALUES ($c :: evt_observe_ctrl,
                   ${sid.toString},
                   $why)
    """.update.run

  def insertAbortObserve(sid: Sequence.Id, why: Option[String]): ConnectionIO[Int] =
    insertObserveCtrl(Ctrl.Abort, sid, why)

  def insertContinueObserve(sid: Sequence.Id, why: Option[String]): ConnectionIO[Int] =
    insertObserveCtrl(Ctrl.Continue, sid, why)

  def insertPauseObserve(sid: Sequence.Id, why: Option[String]): ConnectionIO[Int] =
    insertObserveCtrl(Ctrl.Pause, sid, why)

  def insertStopObserve(sid: Sequence.Id, why: Option[String]): ConnectionIO[Int] =
    insertObserveCtrl(Ctrl.Pause, sid, why)

  private def insertObserveIntegration(l: Lifecycle, sid: Sequence.Id, step: Int): ConnectionIO[Int] =
    sql"""
      INSERT INTO log_observe_int (lifecycle, sequence_id, step)
           VALUES ($l :: evt_lifecycle,
                   ${sid.toString},
                   $step)
    """.update.run

  def insertStartIntegration(sid: Sequence.Id, step: Int): ConnectionIO[Int] =
    insertObserveIntegration(Lifecycle.Start, sid, step)

  def insertEndIntegration(sid: Sequence.Id, step: Int): ConnectionIO[Int] =
    insertObserveIntegration(Lifecycle.End, sid, step)

  private def ts(i: Instant): Timestamp =
    Timestamp.from(i)

  private def selectAllObserveStage(start: Instant, end: Instant): ConnectionIO[List[EventLog.Event]] =
    sql"""
      SELECT timestamp,
             lifecycle,
             observe_stage,
             sequence_id
        FROM log_observe_stage
       WHERE timestamp BETWEEN ${ts(start)} AND ${ts(end)}
    ORDER BY timestamp
    """.query[(Timestamp, Lifecycle, Stage, String)].map { case (t,l,s,i) =>
      val timestamp = t.toInstant
      val sid       = Sequence.Id.unsafeFromString(i)
      val start     = l == Lifecycle.Start

      s match {
        case Slew   => start ? (EventStartSlew(timestamp, sid):     Event) | EventEndSlew(timestamp, sid)
        case Visit  => start ? (EventStartVisit(timestamp, sid):    Event) | EventEndVisit(timestamp, sid)
        case Seq    => start ? (EventStartSequence(timestamp, sid): Event) | EventEndSequence(timestamp, sid)
      }
    }.list

  private def selectAllObserveControl(start: Instant, end: Instant): ConnectionIO[List[EventLog.Event]] =
    sql"""
      SELECT timestamp,
             observe_ctrl,
             sequence_id,
             why
        FROM log_observe_ctrl
       WHERE timestamp BETWEEN ${ts(start)} AND ${ts(end)}
    ORDER BY timestamp
    """.query[(Timestamp, Ctrl, String, Option[String])].map { case (t,c,i,w) =>
      val timestamp = t.toInstant
      val sid       = Sequence.Id.unsafeFromString(i)

      c match {
        case Ctrl.Abort    => EventAbortObserve(   timestamp, sid, w): Event
        case Ctrl.Continue => EventContinueObserve(timestamp, sid, w): Event
        case Ctrl.Pause    => EventPauseObserve(   timestamp, sid, w): Event
        case Ctrl.Stop     => EventStopObserve(    timestamp, sid, w): Event
      }
    }.list

  private def selectAllObserveIntegration(start: Instant, end: Instant): ConnectionIO[List[EventLog.Event]] =
    sql"""
      SELECT timestamp,
             lifecycle,
             sequence_id,
             step
        FROM log_observe_int
       WHERE timestamp BETWEEN ${ts(start)} AND ${ts(end)}
    ORDER BY timestamp
    """.query[(Timestamp, Lifecycle, String, Int)].map { case (t,l,i,s) =>
      val timestamp = t.toInstant
      val sid       = Sequence.Id.unsafeFromString(i)

      l match {
        case Lifecycle.Start => EventStartIntegration(timestamp, sid, s): Event
        case Lifecycle.End   => EventEndIntegration(  timestamp, sid, s): Event
      }
    }.list

  def selectAll(start: Instant, end: Instant): ConnectionIO[List[EventLog.Event]] =
    for {
      c <- selectAllObserveControl(start, end)
      i <- selectAllObserveIntegration(start, end)
      s <- selectAllObserveStage(start, end)
    } yield (c ++ i ++ s).sortBy(_.timestamp)

}
