# Seqexec engine cats/fs2 prototype

## Goals and status

- Get hands on expertise in libraries we'd to migrate to: `cats`, `cats-effect`,
  `fs2`, `monocle`. This is also useful to better assess future porting efforts.

- A new model, that needs to be further developed, optimized for manipulation
  instead of execution. Pending Steps are not wrapped in `Task`s (or now the
  equivalent `Effect` type class) so they are easier to modify. Each type of
  Step, when in *Pending* position only, comes with `_.execute` method where the
  actual execution for that particular Step. From the application State there is
  a Prism to this method that indicates when the `execute` method is there. So
  it's only possible to perform an execution when the whole State is in the
  right position.

- Synchronous-like control of execution even though there is asynchronicity
  under the hood where needed. What to do after an execution is decided where
  the execution takes place, there are no system events that need to re-handled
  from the top.

- No `Process`es (or `fs2` `Stream`) are used internally, only the new
  equivalent of `Task`s in `cats-effect`: the `Effect` type class. Monad
  transformers like `StateT` or `EitherT` are not needed for the core
  functionality.

- Concurrent shared state through `fs2` `Signal`s. This is a wrapper for the
  `Ref`, the fundamental concurrently safe mutable variable in `fs2`. `Signal`
  adds the possibility of obtaining an output stream with every State update.

- Error handling: There is now a hint of how to handle errors from subsystems
  but this would need to be further developed with the introduction of failed
  `Step`s where the *correct* data is still there, and the error is pointed
  exactly where it happened within the `Step`. I had the intention of showing
  Some examples of traditional exception handling from the top of a `Process`
  (now `Stream`s) but is still missing.

- ADTs in anger. This is something that I explored without having a clear idea
  beforehand. For example, in a GMOS Sequence it should be possible only to run
  GMOS Steps (which have their own distinct type different to F2 Steps). There
  is also ADTs for differentiate between Steps: Pending, Current, Done Steps are
  distinct types, so it should be impossible to do things like executing a Done
  Step, or putting a Pending Step in the list of Done Steps. Because there is a
  lot of shared data between the different types of Steps there is also the
  concept of `core` Step which should be used internally for every type of Step.

  However, in order to be able to use comfortably these deep ADTs several
  lens/prisms would need to be written. I'm also not satisfied of how I abused
  Scala objects as namespaces which in hindsight looks messy so I'm looking for
  simpler solution to encode deeply nested ADTs in Scala.

- Concurrency encapsulated in its own object. `fs2` `Signal`s provides a good
  enough set of primitives, so that object is mostly wrappers of those with
  specialized signatures for the engine so it feels more natural. Many of the
  necessary wrappers are still missing but eventually, if further developed, it
  should serve as a mini-DSL to define Step executions easily.

## Effort evaluation of what is missing

 * Execution DSL: A set of helper functions to be used as a *mini-language* to
   define Step executions easily where signal updates are hidden. The code
   defining the step executions should not know anything about updating the
   state.

 * Sequence Loading: Reading from the ODB should be easy but the keywords to the new
   model would take some effort.

 * Printing State: The internal model would have to be converted to the state
   needed by the UI. Under the hood it all follows the pattern of
   zipping/unzipping which is what we are using now, but because the internal
   model is now completely different it would have to be written from scratch
   (arguably, without `Task`s in the Pending Steps it should be easier though)

 * Breakpoints: This should be easy as it is now.

 * Multiple Sequences: This would be the same as the current engine with a map
   of Sequences. Most of the work here would be direct port of what we have to
   `fs2`/`cats-effect`. For example, using `fs2` `join`, instead of
   `scalaz-stream` `mergeN` for each Sequence `Process`/`Stream`. I'm not
   entirely sure `join` and `mergeN` are semantically equivalent so this task
   may become more difficult than it seems.

 * Parallel Sequences: This would be the same as of now with the concept of
   `Resources` but done at the Step level. It should be trivial.

 * Lens/Prism. Many more lens than what we have now would need to be defined in
   order to work effectively with the new model. It would be cumbersome to
   define them but once there, maintenance shouldn't be a problem and the
   consumer code would be very easy to write, provided we are familiar with the
   concept of Lens/Prism (which in our team is fine) but it could be a
   considerable knowledge barrier if we want to involve more people.
   
 * Tests: Our current tests are not that comprehensive so many of them would be
   not worth porting. Most of the tests would have to be written from scratch
   but this would be the task that would take the most time of all the listed
   above.
