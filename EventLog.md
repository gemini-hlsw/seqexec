# Event Log

The "Event Log" is a collection of tables that record observation execution events over the course of an observing session.  In today's model these events are kept in the science program model itself, distributed amongst the observations to which they correspond.  In the future I assume that they will be kept in relational database tables along with everything else.  This document contains a few notes exploring bringing today's log into the new world.  There are likely other event types of interest that could be incorporated in the future.

## Sequences and Dataset Labels

In today's model we're limited to a single sequence per observation, but this is far from ideal.  In particular a commonly mentioned goal is to support both acquisition and science sequences in a single observation, sharing all other details such as target, guide stars, etc.  Yet support for multiple sequences poses a compatibility problem because as soon as there exists more than one sequence, a way to distinguish sequences is necessary. Currently dataset labels consist of the following parts:

````
PROGID-OBSERVATION#-DATASET#
````

where the `PROGID` has its own internal fields and the `DATASET#` has a minimum of 3 digits with leading zeros as necessary.  For example, Gemini North 2017A queue program 1, observation 2, dataset/step 3 is represented with

````
GN-2017A-Q-1-2-003
````

Presumably countless python-based (i.e., maintenance-challenged) scripts have been written assuming that format.  If we introduce some kind of sequence identification between the observation number and the dataset number, they will all need to be updated.

Now add to this complexity the fact that new instruments will generate multiple (maybe even an *unknown* number of) datasets per sequence step.  Either the familiar dataset label has to change or else we have to give up on the idea of mapping datasets to individual sequence steps via the dataset label itself.  I'll assume the later and so a query will have to be performed if we want to map a label to a particular step in a particular sequence of an observation.  (Or perhaps I'm wrong?  Maybe all he python scripts work with the filename format which doesn't reference the dataset label at all?)

In order to "perform a query" a dataset table obviously has to exist in the model.  I'll assume that the seqexec will be able to determine which datasets are associated with the step it just asked the instrument to observe.  These can be written into a dataset table containing columns for dataset id, sequence id, step number, creation time, filename, etc.

Regardless, a sequence id concept is necessary.  It isn't clear if acquisition vs. science are the only kinds of sequences that need to be supported, whether there can be multiple science sequences, etc.  If we could enumerate the kinds of sequences we could tighten the format of sequence ids and prevent errors. I'll assume for now though an open `String` that can contain anything except hyphen characters, `-`, since they are field separators in our labels.

````scala
package gem

import scalaz._
import Scalaz._

object Sequence {

  trait Id {
    def oid: Observation.Id
    def name: String
  }

  object Id extends ((Observation.Id, String) => Option[Sequence.Id]) {
    def apply(o: Observation.Id, n: String): Option[Sequence.Id] =
      !(n.contains('-') || "" === n.trim) option new Sequence.Id {
        override val oid: Observation.Id = o
        override val name: String        = n.trim

        override def toString: String =
          s"${oid.toString}-$n"
      }

    def fromString(s: String): Option[Sequence.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          Observation.Id.fromString(a).flatMap { Id(_, b.drop(1)) }
      }

    def unsafeFromString(s: String): Sequence.Id =
      fromString(s) | sys.error(s"Malformed Sequence.Id: $s")

    def unapply(arg: Id): Option[(Observation.Id, String)] =
      Some((arg.oid, arg.name))
  }

}
````

## Observation Execution Events

In today's OCS, the tcc and seqexec send observation execution events to an intermediary process called the "wdba" as they slew to targets and execute sequences.  Perhaps in the future these tools will write directly to the database?  At any rate, all the events share a timestamp and an observation id but some types of events need additional information.  In the multi-sequence future, the observation id should be replaced with a sequence id for all (or maybe just most) events.

These event types correspond, roughly, to what we have today:

````scala
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


  case class EventPauseObserve(   timestamp: Instant, sid: Sequence.Id, why: Option[String]) extends Event
  case class EventContinueObserve(timestamp: Instant, sid: Sequence.Id, why: Option[String]) extends Event
  case class EventAbortObserve(   timestamp: Instant, sid: Sequence.Id, why: Option[String]) extends Event
  case class EventStopObserve(    timestamp: Instant, sid: Sequence.Id, why: Option[String]) extends Event

  case class EventStartIntegration(timestamp: Instant, sid: Sequence.Id, step: Int) extends Event
  case class EventEndIntegration(  timestamp: Instant, sid: Sequence.Id, step: Int) extends Event
}
````

Here I replaced "dataset" start/end events with "integration" start/end keeping in mind that multiple datasets may be produced for each step.

As indicated by vertical whitespace in the code above there are three categories of events.  Correspondingly, I assume there will need to be three tables to store them (?).

### 1. Observe Stage Events

Stages of observation execution: slew, visit, and sequence.  Visit is a concept that I'm not sure I understand.  Is there a difference between visit start/end and sequence start/end?  A couple of `ENUM` types will be necessary to define the table.  First, the event lifecycle to mark the start and end events:

````sql
CREATE TYPE evt_lifecycle AS ENUM (
    'Start',
    'End'
);

ALTER TYPE evt_lifecycle OWNER TO postgres;
````

Which are translated into generated Scala enum types in `gen2`:

````scala
      enum("EventLifecycle") {
        type EventLifecycleRec = Record.`'tag -> String`.T
        val io = sql"""
          SELECT enumlabel a, enumlabel b
          FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
          WHERE pg_type.typname = 'evt_lifecycle'
        """.query[(String, EventLifecycleRec)].list
        io.transact(xa).unsafePerformIO
      }
````

Second, the stage type itself:

````sql
CREATE TYPE evt_observe_stage AS ENUM (
    'Sequence',
    'Slew',
    'Visit'
);

ALTER TYPE evt_observe_stage OWNER TO postgres;
````

with corresponding code to generate a Scala `EventObserveStage` enum.  Finally, the table itself:

````sql
CREATE TABLE log_observe_stage
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   lifecycle     evt_lifecycle NOT NULL,
   observe_stage evt_observe_stage NOT NULL,
   sequence_id   text NOT NULL
);
 
ALTER TABLE log_observe_stage OWNER TO postgres;

ALTER TABLE ONLY log_observe_stage
    ADD CONSTRAINT log_observe_stage_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_stage_timestamp ON log_observe_stage USING btree ("timestamp" DESC);
````

There are a couple of things I'm not sure about here.  First, `timestamp`, even in combination with sequence id, doesn't seem like a valid primary key.  Do we need a primary key at all?  We'll never actually join on the primary key, I think. The reason I could find for it was avoiding duplicates when something goes wrong reading from a backup.

Next, the timestamp itself is a `timestamp with time zone`.  It took me a while of floundering to find something that would work as expected, since I was naturally drawn to `timestamp without time zone`.  As I understand it tough, the timestamp is stored properly as an instance from a standard epoch in UTC.  The `with time zone` is just about how time input is interpreted?

Finally, I made an index on the `timestamp` since that supports a major use case -- finding events between two given times.  I think we'll also want an index on the sequence id since I anticipate wanting all the events for a given sequence or observation.

### 2. Observe Control Events

Observe control refers to pause/continue, stop/abort actions.  The corresponding `ENUM` is:

````sql
create TYPE evt_observe_ctrl AS ENUM (
    'Abort',
    'Continue',
    'Pause',
    'Stop'
);

ALTER TYPE evt_observe_ctrl OWNER TO postgres;
````

and the table itself is

````sql
CREATE TABLE log_observe_ctrl
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   observe_ctrl  evt_observe_ctrl NOT NULL,
   sequence_id   text NOT NULL,
   why           text
);
 
ALTER TABLE log_observe_ctrl OWNER TO postgres;

ALTER TABLE ONLY log_observe_ctrl
    ADD CONSTRAINT log_observe_ctrl_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_ctrl_timestamp ON log_observe_ctrl USING btree ("timestamp" DESC);
````

### 3. Observe Integration Events

Integration refers to the entire time data is being collected for a given step.  For future instruments, this may produce multiple datasets so I renamed the old "dataset" event.  Here I don't try to include the actual datasets that are created but rather anticipate a separate `dataset` table that can be joined via the `sequence_id` and `step` number.

````sql
CREATE TABLE log_observe_int
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   lifecycle     evt_lifecycle NOT NULL,
   sequence_id   text NOT NULL,
   step          integer NOT NULL
);
 
ALTER TABLE log_observe_int OWNER TO postgres;

ALTER TABLE ONLY log_observe_int
    ADD CONSTRAINT log_observe_int_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_int_timestamp ON log_observe_int USING btree ("timestamp" DESC);
````

### Determining the Current State

From the information in the event logs, we should be able to determine:

1. which steps are executed and in which order across the sequences
2. today's time accounting information
3. which step is currently executing, if any

I don't think there is a need to move executed steps into a separate log of any kind? Though we're relying on code to avoid rewriting history I suppose.


## Event Log DAO

The DAO provides simple methods for inserting the various event types into the corresponding tables and for generating a `List[EventLog.Event]` from the tables as a proof of concept:

````scala
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
````

I'm just combining the data collected from the three tables in Scala code.  Maybe there's a more efficient way to do this in a query?
