# Seqexec engine cats/fs2 prototype

## Goals and status

- Get hands on expertise in libraries we'd to migrate to: `cats`, `cats-effect`,
  `fs2`, `monocle`. This is also useful to better assess future porting efforts.
- A new model, that needs to be further developed, optimized for manipulation
  instead of execution. Pending Steps are not wrapped in `Task`s so they are
  easier to modify. Each type of Step comes with `_.execute` method where the
  actual execution is defined.
- Synchronous-like control of execution even though there is asynchronicity
  under the hood where needed. What to do after an execution is decided where
  the execution takes place, there are no system events that need to re-handled
  from the top.
- No `Process`es (or fs2 `Stream`) are used internally, only the new equivalent
  of `Task`s in fs2/cats: the `Effect` type class. Monad transformers like
  `StateT` or `EitherT` are not needed now.
- Concurrent shared state through `fs2` `Signal`s. This is a wrapper for the
  `Ref`, the fundamental concurrently safe mutable variable in `fs2`. `Signal`
  adds the possibility of obtaining an output stream with every state update.
- Error handling: There is now a hint of how to handle errors from subsystems
  but this needs to be further developed. Some examples of how to handle
  exceptions also need to be provided.
- At least a single test needs to be written showing the prototype can run a
  dummy sequence properly.

## Out of scope

 * Execution DSL: A set of helper functions to be used as a *mini-language* to
   define Step executions easily where signal updates are hidden. The code
   defining the step executions should not know anything about updating the
   state. 4-5 days.
 * Loading: Reading from the ODB would be easy but converting it to the new
   model would take some effort: ~4-5 days.
 * Printing State: The internal model would have to be converted to the state
   needed by the UI. 3-4 days.
 * Breakpoints: This should be easy now. 1 day.
 * Multiple Sequences: This would be the same as the real seqexec with a map
   of Sequences. Most of the work here would be direct port of what we have to
   `fs2`/`cats-effect`. For example, using `join`, instead of `mergeN` for each
   Sequence `Process`/`Stream`. 2 days.
 * Parallel Sequences: This would be the same as of now with the concept of
   `Resources` but done at the Step level. 1 day
 * Tests: Our current tests are not that comprehensive so many of them would be
   not worth porting. Most of the tests would have to be written from scratch.
   4-5 days.
