package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Event._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.Sink
import scalaz.stream.async.mutable.{Queue => SQueue}

package object engine {

  // Top level synonyms

  /**
    * This represents an actual real-world action to be done in the underlying
    * systems.
    */
  type Action = Task[Result]

  // Avoid name class with the proper Seqexec `Queue`
  type EventQueue = SQueue[Event]

  /**
    * An `Execution` is a group of `Action`s that need to be run in parallel
    * without interruption. A *sequential* `Execution` can be represented with
    * an `Execution` with a single `Action`.
    */
  // TODO: This should be a `NonEmptyList`. `Current.results`, `Current.actions`
  // would still need to be plain `List`s.
  type Execution[A] = List[A]

  // Engine proper

  /**
    * Type constructor where all Seqexec side effect are managed.
    */
  type Engine[A] = EngineStateT[Task, A]
  // Helper alias to facilitate lifting.
  type EngineStateT[M[_], A] = StateT[M, QState, A]

  /**
    * Changes the `Status` and returns the new `QState`.
    *
    * It also takes care of initiating the execution when transitioning to
    * `Running` `Status`.
    */
  def switch(q: EventQueue)(st: Status): Engine[Unit] =
    modify(QState.status.set(_, st)) *>
    // TODO: Make Status an Equal instance
    whenM (st == Status.Running) (execute(q))

  /**
    * Adds the current Execution` to the completed `Queue`, makes the next
    * pending `Execution` the current one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  def next(q: EventQueue): Engine[Unit] =
    gets(_.next).flatMap {
      // No more Executions left
      case None     => send(q)(finished)
      // Execution completed, execute next actions
      case Some(qs) => put(qs) *> execute(q)
    }

  /**
    * Checks the `Status` is `Running` and executes all actions in the `Current`
    * `Execution` in parallel. When all are done it emits the `Executed` event.
    * It also updates the `State` as needed.
    */
  private def execute(q: EventQueue): Engine[Unit] = {

    // Send the expected event when the `Action` is executed
    def act(t: (Action, Int)): Task[Unit] = t match {
      case (action, i) =>
        action.flatMap {
          case Result.OK(r)    => q.enqueueOne(completed(i, r))
          case Result.Error(e) => q.enqueueOne(failed(i, e))
        }
    }

    status.flatMap {
      case Status.Waiting   => unit
      case Status.Completed => unit
      case Status.Running   => (
        gets(_.current.actions).flatMap(
          actions => Nondeterminism[Task].gatherUnordered(
            actions.zipWithIndex.map(act)
          ).liftM[EngineStateT]
        )
      ) *> send(q)(executed)
    }
  }

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exit it does nothing.
    */
  def complete[R](i: Int, r: R): Engine[Unit] = modify(_.mark(i)(Result.OK(r)))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  def fail[E](q: EventQueue)(i: Int, e: E): Engine[Unit] =
    modify(_.mark(i)(Result.Error(e))) *> switch(q)(Status.Waiting)

  /**
    * Ask for the current Engine `Status`.
    */
  val status: Engine[Status] = gets(_.status)

  /**
    * Log something and return the `State`.
    */
  // XXX: Proper Java logging
  def log(msg: String): Engine[Unit] = pure(println(msg))

  /** Terminates the `Engine` returning the final `State`.
    */
  def close(queue: EventQueue): Engine[Unit] = queue.close.liftM[EngineStateT]

  /**
    * Enqueue `Event` in the Engine.
    */
  private def send(q: EventQueue)(ev: Event): Engine[Unit] = q.enqueueOne(ev).liftM[EngineStateT]

  // Functions for type bureaucracy

  /**
    * This creates an `Event` Process with `Engine` as effect.
    */
  def receive(queue: EventQueue): Process[Engine, Event] = hoistEngine(queue.dequeue)

  private def pure[A](a: A): Engine[A] = Applicative[Engine].pure(a)

  private val unit: Engine[Unit] = pure(Unit)

  val get: Engine[QState] =
    MonadState[Engine, QState].get

  private def gets[A](f: (QState) => A): Engine[A] =
    MonadState[Engine, QState].gets(f)

  private def modify(f: (QState) => QState) =
    MonadState[Engine, QState].modify(f)

  private def put(qs: QState): Engine[Unit] =
    MonadState[Engine, QState].put(qs)

  // The `Catchable` instance of `Engine`` needs to be manually written.
  // Without it's not possible to use `Engine` as a scalaz-stream process effects.
  implicit val engineInstance: Catchable[Engine] =
    new Catchable[Engine] {
      def attempt[A](a: Engine[A]): Engine[Throwable \/ A] = a.flatMap(
        x => Catchable[Task].attempt(Applicative[Task].pure(x)).liftM[EngineStateT]
      )
      def fail[A](err: Throwable) = Catchable[Task].fail(err).liftM[EngineStateT]
    }

  /**
    * Lifts from `Task` to `Engine` as the effect of a `Process`.
    */
  def hoistEngine[A](p: Process[Task, A]): Process[Engine, A] = {
    val toEngine = new (Task ~> Engine) {
      def apply[B](t: Task[B]): Engine[B] = t.liftM[EngineStateT]
    }
    p.translate(toEngine)
  }

  /**
    * Lifts from `Task` to `Engine` as the effect of a `Sink`.
    */
  def hoistEngineSink[O](s: Sink[Task, O]): Sink[Engine, O] =
    hoistEngine(s).map(_.map(_.liftM[EngineStateT]))
}
