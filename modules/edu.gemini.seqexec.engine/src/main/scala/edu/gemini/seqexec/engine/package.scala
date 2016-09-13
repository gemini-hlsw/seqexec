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
  type Execution[A] = NonEmptyList[A]

  // Engine proper

  /**
    * Type constructor where all Seqexec side effect are managed.
    */
  type Engine[A] = EngineStateT[Task, A]
  // Helper alias to facilitate lifting.
  type EngineStateT[M[_], A] = StateT[M, State, A]

  /**
    * Changes the `Status` and returns the new `State`.
    *
    * It also takes care of initiating the execution when transitioning to
    * `Running` status.
    */
  def switch(q: EventQueue)(st: Status): Engine[State] =
    modify (State.status.set(_, st)) *> { if (st == Status.Running) next(q) else get }

  /**
    * Adds the `Current` `Execution` to the completed `Queue`, makes the next
    * pending `Execution` the `Current` one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  // XXX: Handle trying to `next` when there are actions pending in Current.
  def next(q: EventQueue): Engine[State] =
    (gets(State.next(_)) >>= {
       // This should only happen when Queue is empty.
       case None => send(q)(finished)
       case Some((actions, s)) => put(s) *> execute(q)(actions)
     }) *> get

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new `State`.
    *
    * When the index doesn't exit it does nothing.
    */
  def complete(i: Int): Engine[State] = modify(State.mark(i)(Result.OK)(_)) *> get

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  def fail(q: EventQueue)(i: Int): Engine[State] =
    modify(State.mark(i)(Result.Error)(_)) *> switch(q)(Status.Waiting)

  /**
    * Adds an `Execution` to the beginning of the `QueueStatus` while returning
    * the `QueueStatus`.
    */
  // TODO: Change this to the end or insert by index. For that List -> Vector in
  // `QueueStatus`
  // def add(pend: Execution[Action]): Engine[State] = {
  //   val l = QueueStatus.pending.partial >=>
  //     PLens.listHeadPLens[Sequence.Pending] >=>
  //     PLens.listHeadPLens[Step.Pending]
  //   modify(l.mod((pend :: _), _)) *> get
  // }

  /**
    * Ask for the current Engine `Status`.
    */
  val status: Engine[Status] = gets(_.status)

  /**
    * Log something and return the `State`.
    */
  // XXX: Proper Java logging
  def log(msg: String): Engine[State] = pure(println(msg)) *> get

  /** Terminates the `Engine` returning the final `State`.
    */
  def close(queue: EventQueue): Engine[State] =
    queue.close.liftM[EngineStateT] *> get

  /**
    * Enqueue `Event` in the Engine.
    */
  private def send(q: EventQueue)(ev: Event): Engine[Unit] = q.enqueueOne(ev).liftM[EngineStateT]

  /**
    * Checks the `Status` is `Running` and executes all actions in the `Current`
    * `Execution` in parallel. When all are done it emits the `Executed` event.
    * It also updates the `State` as needed.
    */
  private def execute(q: EventQueue)(actions: Execution[Action]): Engine[Unit] = {

    // Send the expected event when action is executed
    def act(t: (Action, Int)): Task[Unit] = {
      val (action, i) = t
      action >>= {
        case Result.OK => q.enqueueOne(completed(i))
        case Result.Error => q.enqueueOne(failed(i))
      }
    }

    status >>= {
      case Status.Running => (
        Nondeterminism[Task].gatherUnordered(
          actions.toList.zipWithIndex.map(act(_)
          )
        ).liftM[EngineStateT]
      ) *> send(q)(executed)
      case Status.Waiting => unit
    }
  }

  // Functions to facilitate type bureaucracy

  /**
    * This creates a `Event` Process with `Engine` as effect.
    */
  def receive(queue: EventQueue): Process[Engine, Event] = hoistEngine(queue.dequeue)

  private def pure[A](a: A): Engine[A] = Applicative[Engine].pure(a)

  private val unit: Engine[Unit] = pure(Unit)

  private val get: Engine[State] =
    MonadState[Engine, State].get

  private def gets[A](f: (State) => A): Engine[A] =
    MonadState[Engine, State].gets(f)

  private def modify(f: (State) => State) =
    MonadState[Engine, State].modify(f)

  private def put(qs: State): Engine[Unit] =
    MonadState[Engine, State].put(qs)

  // The `Catchable` instance of `Engine`` needs to be manually written.
  // Without it's not possible to use `Engine` as a scalaz-stream process effects.
  implicit val engineInstance: Catchable[Engine] =
    new Catchable[Engine] {
      def attempt[A](a: Engine[A]): Engine[Throwable \/ A] = a >>= (
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
