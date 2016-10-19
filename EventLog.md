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


  case class EventPauseObserve(   timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventContinueObserve(timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventAbortObserve(   timestamp: Instant, sid: Sequence.Id) extends Event
  case class EventStopObserve(    timestamp: Instant, sid: Sequence.Id) extends Event

  case class EventStartIntegration(timestamp: Instant, sid: Sequence.Id, step: Int) extends Event
  case class EventEndIntegration(  timestamp: Instant, sid: Sequence.Id, step: Int) extends Event
}
````

Here I replaced "dataset" start/end events with "integration" start/end keeping in mind that multiple datasets may be produced for each step.


### Observe Events

Observe events can be mapped to a single table with a few constraints to
ensure consistency.  First, an `ENUM`  will be necessary to define the
event type:


````scala
CREATE TYPE evt_type AS ENUM (
    'StartSequence',
    'EndSequence',
    'StartSlew',
    'EndSlew',
    'StartVisit',
    'EndVisit',
    'StartIntegration',
    'EndIntegration',
    'Abort',
    'Continue',
    'Pause',
    'Stop'
);
````

Which are translated into generated Scala enum types in `gen2`:

````scala
       enum("EventType") {
         type EventTypeRec = Record.`'tag -> String`.T
         val io = sql"""
           SELECT enumlabel a, enumlabel b
           FROM pg_enum JOIN pg_type ON pg_enum.enumtypid = pg_type.oid
           WHERE pg_type.typname = 'evt_type'
         """.query[(String, EventTypeRec)].list
         io.transact(xa).unsafePerformIO
       }
````

Then the table itself:

````scala
CREATE TABLE log_observe_event
(
   id            SERIAL,
   "timestamp"   timestamp (5) WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
   event         evt_type NOT NULL,
   sequence_id   text NOT NULL,
   step          integer CHECK (step > 0),
   CONSTRAINT check_step CHECK ((event IN ('StartIntegration', 'EndIntegration') AND step iS NOT NULL) OR
                                (event NOT IN ('StartIntegration', 'EndIntegration') AND step IS NULL))
);

ALTER TABLE log_observe_event OWNER TO postgres;

ALTER TABLE ONLY log_observe_event
    ADD CONSTRAINT log_observe_event_pkey PRIMARY KEY (id);

CREATE INDEX ix_log_observe_event_timestamp ON log_observe_event USING btree ("timestamp" DESC);
````

Note the constraints on step to guarantee a positive value that is defined
if and only if it is an integration event.

There are a couple of things I'm not sure about here.  First, `timestamp`, even in combination with sequence id, doesn't seem like a valid primary key.  Do we need a primary key at all?  We'll never actually join on the primary key, I think. The reason I could find for it was avoiding duplicates when something goes wrong reading from a backup.

Next, the timestamp itself is a `timestamp with time zone`.  It took me a while of floundering to find something that would work as expected, since I was naturally drawn to `timestamp without time zone`.  As I understand it tough, the timestamp is stored properly as an instance from a standard epoch in UTC.  The `with time zone` is just about how time input is interpreted?

Finally, I made an index on the `timestamp` since that supports a major use case -- finding events between two given times.  I think we'll also want an index on the sequence id since I anticipate wanting all the events for a given sequence or observation.

### Determining the Current State

From the information in the event log, we should be able to determine:

1. which steps are executed and in which order across the sequences
2. today's time accounting information
3. which step is currently executing, if any

I don't think there is a need to move executed steps into a separate log of any kind? Though we're relying on code to avoid rewriting history I suppose.


## Event Log DAO

The DAO provides simple methods for inserting the various event types into the corresponding tables and for generating a `List[EventLog.Event]` from the tables as a proof of concept:

````scala
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
````
