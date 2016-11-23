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

  type Actions = List[Action]

  type Results = List[Result]

  // Handle proper

  /**
    * Type constructor where all Seqexec side effect are managed.
    *
    * It's named `Handle` after `fs2.Handle` in order to facilitate a future
    * migration.
    */
  type Handle[A] = HandleStateT[Task, A]
  // Helper alias to facilitate lifting.
  type HandleStateT[M[_], A] = StateT[M, Queue.State, A]

  /**
    * Changes the `Status` and returns the new `Queue.State`.
    *
    * It also takes care of initiating the execution when transitioning to
    * `Running` `Status`.
    */
  def switch(q: EventQueue)(st: Status): Handle[Unit] =
    // TODO: Make Status an Equal instance
    modify(Queue.State.status.set(_, st)) *> whenM(st == Status.Running)(next(q))

  /**
    * Reloads the (for now only) sequence
    */
  def load(seq: Sequence[Action]): Handle[Unit] = status.flatMap {
    case Status.Running => unit
    case _ => put(Queue.State.init(engine.Queue(List(seq))))
  }

  /**
    * Adds the current Execution` to the completed `Queue`, makes the next
    * pending `Execution` the current one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  def next(q: EventQueue): Handle[Unit] =
    gets(_.next).flatMap {
      // Empty state
      case None     => send(q)(finished)
      // Final State
      case Some(qs: Queue.State.Final) => put(qs) *> send(q)(finished)
      // Execution completed, execute next actions
      case Some(qs) => put(qs) *> execute(q)
    }

  /**
    * Checks the `Status` is `Running` and executes all actions in the `Current`
    * `Execution` in parallel. When all are done it emits the `Executed` event.
    * It also updates the `State` as needed.
    */
  private def execute(q: EventQueue): Handle[Unit] = {

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
          ).liftM[HandleStateT]
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
  def complete[R](i: Int, r: R): Handle[Unit] = modify(_.mark(i)(Result.OK(r)))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  def fail[E](q: EventQueue)(i: Int, e: E): Handle[Unit] =
    modify(_.mark(i)(Result.Error(e))) *> switch(q)(Status.Waiting)

  /**
    * Ask for the current Handle `Status`.
    */
  val status: Handle[Status] = gets(_.status)

  /**
    * Log something and return the `State`.
    */
  // XXX: Proper Java logging
  def log(msg: String): Handle[Unit] = pure(println(msg))

  /** Terminates the `Handle` returning the final `State`.
    */
  def close(queue: EventQueue): Handle[Unit] = queue.close.liftM[HandleStateT]

  /**
    * Enqueue `Event` in the Handle.
    */
  private def send(q: EventQueue)(ev: Event): Handle[Unit] = q.enqueueOne(ev).liftM[HandleStateT]

  /**
    * Main logical thread to handle events and produce output.
    */
  private def run(q: EventQueue)(ev: Event): Handle[Queue.State] = {

    def handleUserEvent(ue: UserEvent): Handle[Unit] = ue match {
      case Start              =>
        log("Output: Started") *> switch(q)(Status.Running)
      case Pause              =>
        log("Output: Paused") *> switch(q)(Status.Waiting)
      case Load(seq) => log("Output: Sequence loaded") *> load(seq)
      case Poll               =>
        log("Output: Polling current state")
      case Exit               =>
        log("Bye") *> close(q)
    }

    def handleSystemEvent(se: SystemEvent): Handle[Unit] = se match {
      case (Completed(i, r)) =>
        log("Output: Action completed") *> complete(i, r)
      case (Failed(i, e))    =>
        log("Output: Action failed") *> fail(q)(i, e)
      case Executed          =>
        log("Output: Execution completed, launching next execution") *> next(q)
      case Finished          =>
        log("Output: Finished") *> switch(q)(Status.Completed)
    }

    (ev match {
        case EventUser(ue)   => handleUserEvent(ue)
        case EventSystem(se) => handleSystemEvent(se)
      }) *> get
  }

  // Kudos to @tpolecat
  /** Traverse a process with a stateful computation. */
  private def mapEvalState[F[_]: Monad: Catchable, A, S, B](
    fs: Process[F, A], s: S, f: A => StateT[F, S, B]
  ): Process[F, B] = {
    def go(fs: Process[F, A], s: S): Process[F, B] =
      Process.eval(fs.unconsOption).flatMap {
        case None         => Process.halt
        case Some((h, t)) => Process.eval(f(h).run(s)).flatMap {
          case (s, a) => Process.emit(a) ++ go(t, s)
        }
      }
    go(fs, s)
  }

  def runE(q: EventQueue)(ev: Event): Handle[(Event, Queue.State)] =
    run(q)(ev).map((ev, _))

  def processE(q: EventQueue): Process[Handle, (Event, Queue.State)] =
    receive(q).evalMap(runE(q))

  def process(q: EventQueue)(qs: Queue.State): Process[Task, (Event, Queue.State)] = {
    mapEvalState(q.dequeue, qs, runE(q))
  }

  // Functions for type bureaucracy

  /**
    * This creates an `Event` Process with `Handle` as effect.
    */
  def receive(queue: EventQueue): Process[Handle, Event] = hoistHandle(queue.dequeue)

  def pure[A](a: A): Handle[A] = Applicative[Handle].pure(a)

  private val unit: Handle[Unit] = pure(Unit)

  val get: Handle[Queue.State] =
    MonadState[Handle, Queue.State].get

  private def gets[A](f: (Queue.State) => A): Handle[A] =
    MonadState[Handle, Queue.State].gets(f)

  private def modify(f: (Queue.State) => Queue.State) =
    MonadState[Handle, Queue.State].modify(f)

  private def put(qs: Queue.State): Handle[Unit] =
    MonadState[Handle, Queue.State].put(qs)

  // For instrospection
  val printQueueState: Handle[Unit] = gets((qs: Queue.State) => Task.now(println(qs)).liftM[HandleStateT])

  // The `Catchable` instance of `Handle`` needs to be manually written.
  // Without it's not possible to use `Handle` as a scalaz-stream process effects.
  implicit val engineInstance: Catchable[Handle] =
    new Catchable[Handle] {
      def attempt[A](a: Handle[A]): Handle[Throwable \/ A] = a.flatMap(
        x => Catchable[Task].attempt(Applicative[Task].pure(x)).liftM[HandleStateT]
      )
      def fail[A](err: Throwable) = Catchable[Task].fail(err).liftM[HandleStateT]
    }

  /**
    * Lifts from `Task` to `Handle` as the effect of a `Process`.
    */
  def hoistHandle[A](p: Process[Task, A]): Process[Handle, A] = {
    val toHandle = new (Task ~> Handle) {
      def apply[B](t: Task[B]): Handle[B] = t.liftM[HandleStateT]
    }
    p.translate(toHandle)
  }

  /**
    * Lifts from `Task` to `Handle` as the effect of a `Sink`.
    */
  def hoistHandleSink[O](s: Sink[Task, O]): Sink[Handle, O] =
    hoistHandle(s).map(_.map(_.liftM[HandleStateT]))

}
