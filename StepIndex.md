## Step Indices Issue

Sequence steps are modeled in Scala with a simple ADT:

````scala
sealed abstract class Step[A] extends Product with Serializable {
  def instrument: A
}

final case class BiasStep   [A](instrument: A)                             extends Step[A]
final case class DarkStep   [A](instrument: A)                             extends Step[A]
final case class GcalStep   [A](instrument: A, gcal:      GcalConfig)      extends Step[A]
final case class ScienceStep[A](instrument: A, telescope: TelescopeConfig) extends Step[A]
final case class SmartStep  [A](instrument: A, smartCal:  SmartCalConfig)  extends Step[A]
````

The database model mirrors this simple hierarchy with a "base" `step` table:

````sql
CREATE TABLE step (
    observation_id character varying(40) NOT NULL,
    index          smallint              NOT NULL,
    instrument     identifier            NOT NULL,
    step_type      step_type             NOT NULL
);
````

and "sub" tables for the various step types:

````sql
CREATE TABLE step_science (
    index          smallint              NOT NULL,
    observation_id character varying(40) NOT NULL,
    offset_p       double precision      NOT NULL,
    offset_q       double precision      NOT NULL
);
````

Here the primary key of each of the tables is the combination `(observation_id, index)`,
where matching values are used to stitch together the base and sub type rows into a
complete step.  The primary key does double duty, uniquely identifying and also ordering
steps, and yet the order is not fixed.  In fact one of the main selling points of a
flattened sequence model is that it should permit steps to be arranged, reordered, and
updated as necessary.  Imagine for example inserting a step at the beginning of a long
sequence.  This would imply changing the primary key of every step in the sequence that
follows.


## Proposal

The step table needs to track the `observation_id` identifying the observation to which
it belongs but the primary key could be a unique id that is unrelated to the order of
the step. This id then serves as a permanant link between the base and sub tables.

````sql
CREATE TABLE step (
    step_id        SERIAL                PRIMARY KEY,
    observation_id character varying(40) NOT NULL,
    ...
);

CREATE TABLE step_science (
    step_science id SERIAL                PRIMARY KEY REFERENCES step ON DELETE CASADE,
    observation_id  character varying(40) NOT NULL,
    ...
);
```` 

Ordering can take advantage of the Logoot index algorithm that allows steps to
be inserted between any two other steps at any time without the need to update
any other table row.  To do this, a `location` column with type `integer[]` could
be added to the `step` table.  The database orders arrays on an element-by-element
basis such that `[5, 1]` sorts before `[5, 2]` and both before `[10]`.  In fact,
given sufficient spacing between step locations the need to introduce a second
array element will rarely if ever arise.


````sql
CREATE TABLE step (
    step_id        SERIAL                PRIMARY KEY,
    observation_id character varying(40) NOT NULL,
    location       integer[]             NOT NULL,
    instrument     identifier            NOT NULL,
    step_type      step_type             NOT NULL, 
    UNIQUE (observation_id, location)
);
````

Note the `UNIQUE` constraint preventing two steps from residing at the same spot.

