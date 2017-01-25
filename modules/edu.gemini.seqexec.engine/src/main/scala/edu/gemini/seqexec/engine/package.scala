package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Event._
import edu.gemini.seqexec.model.Model.SequenceState

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
  type Actions = List[Action]

  type Results = List[Result]

  type EngineState = Map[Sequence.Id, Sequence.State]
  val initState: EngineState = Map.empty[Sequence.Id, Sequence.State]

  // Handle proper

  /**
    * Type constructor where all Seqexec side effect are managed.
    *
    * It's named `Handle` after `fs2.Handle` in order to facilitate a future
    * migration.
    */
  type Handle[A] = HandleStateT[Task, A]
  // Helper alias to facilitate lifting.
  type HandleStateT[M[_], A] = StateT[M, EngineState, A]

  /**
    * Changes the `Status` and returns the new `Queue.State`.
    *
    * It also takes care of initiating the execution when transitioning to
    * `Running` `Status`.
    */
  def switch(q: EventQueue)(id: Sequence.Id)(st: SequenceState): Handle[Unit] =
    // TODO: Make Status an Equal instance
    modifyS(id)(Sequence.State.status.set(_, st))

  def rollback(q: EventQueue)(id: Sequence.Id): Handle[Unit] =
    modifyS(id)(_.rollback)

  /**
    * Loads a sequence
    */
  def load(id: Sequence.Id, seq: Sequence[Action]): Handle[Unit] =
    StateT[Task, EngineState, Unit] { s =>
      Task {
        s.get(id).map(t => t.status match {
          case SequenceState.Running => (s, ())
          case _                     => (s.updated(id, Sequence.State.init(seq)), ())
        }).getOrElse((s.updated(id, Sequence.State.init(seq)), ()))
      }
    }

  /**
    * Adds the current `Execution` to the completed `Queue`, makes the next
    * pending `Execution` the current one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  def next(q: EventQueue)(id: Sequence.Id): Handle[Unit] =
    getS(id).flatMap(
      _.map { seq =>
        seq.status match {
          case SequenceState.Stopping =>
            seq.next match {
              case None =>
                send(q)(finished(id))
              // Final State
              case Some(qs: Sequence.State.Final) =>
                putS(id)(qs) *> send(q)(finished(id))
              // Execution completed
              case Some(qs) =>
                putS(id)(qs) *> switch(q)(id)(SequenceState.Idle)
            }
          case SequenceState.Running =>
            seq.next match {
              // Empty state
              case None =>
                send(q)(finished(id))
              // Final State
              case Some(qs: Sequence.State.Final) =>
                putS(id)(qs) *> send(q)(finished(id))
              // Execution completed. Check breakpoint here
              case Some(qs) =>
                putS(id)(qs) *> (if(qs.getCurrentBreakpoint) unit else send(q)(executing(id)))
            }
          case _ => unit
        }
    }.getOrElse(unit)
  )

  /**
    * Executes all actions in the `Current` `Execution` in parallel. When all are done it emits the `Executed` event.
    * It also updates the `State` as needed.
    */
  private def execute(q: EventQueue)(id: Sequence.Id): Handle[Unit] = {

    // Send the expected event when the `Action` is executed
    def act(t: (Action, Int)): Task[Unit] = t match {
      case (action, i) =>
        action.flatMap {
          case Result.OK(r)    => q.enqueueOne(completed(id, i, r))
          case Result.Error(e) => q.enqueueOne(failed(id, i, e))
        }
    }

    getS(id).flatMap(
      _.map { seq =>
        seq match {
          case Sequence.State.Final(_,_) => unit
          case _          => Nondeterminism[Task].gatherUnordered (
            seq.current.actions.zipWithIndex.map (act)
            ).liftM[HandleStateT] *> send (q) (executed (id) )
        }
      }.getOrElse(unit)
    )
  }

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exit it does nothing.
    */
  def complete[R](id: Sequence.Id, i: Int, r: R): Handle[Unit] = modifyS(id)(_.mark(i)(Result.OK(r)))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  def fail[E](q: EventQueue)(id: Sequence.Id)(i: Int, e: E): Handle[Unit] =
    modifyS(id)(_.mark(i)(Result.Error(e))) *>
      switch(q)(id)(SequenceState.Error("There was an error"))

  /**
    * Ask for the current Handle `Status`.
    */
  def status(id: Sequence.Id): Handle[Option[SequenceState]] = gets(_.get(id).map(_.status))

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
  private def run(q: EventQueue)(ev: Event): Handle[EngineState] = {

    def handleUserEvent(ue: UserEvent): Handle[Unit] = ue match {
      case Start(id)               => log("Output: Started") *> rollback(q)(id) *>
        switch(q)(id)(SequenceState.Running) *> send(q)(Event.executing(id))
      case Pause(id)               => log("Output: Paused") *> switch(q)(id)(SequenceState.Stopping)
      case Load(id, seq) => log("Output: Sequence loaded") *> load(id, seq)
      case Breakpoint(id, step, v) => log("Output: breakpoint changed") *>
        modifyS(id)(_.setBreakpoint(step, v))
      case Poll                    => log("Output: Polling current state")
      case Exit                    => log("Bye") *> close(q)
    }

    def handleSystemEvent(se: SystemEvent): Handle[Unit] = se match {
      case Completed(id, i, r) => log("Output: Action completed") *> complete(id, i, r)
      case Failed(id, i, e)    => log("Output: Action failed") *> fail(q)(id)(i, e)
      case Executed(id)        => log("Output: Execution completed") *> send(q)(Event.next(id))
      case Executing(id)       => log("Output: Executing") *> execute(q)(id)
      case Next(id)            => log("Output: Moving to next Execution if possible") *> next(q)(id)
      case Finished(id)        => log("Output: Finished") *> switch(q)(id)(SequenceState.Completed)
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

  def runE(q: EventQueue)(ev: Event): Handle[(Event, EngineState)] =
    run(q)(ev).map((ev, _))

  def processE(q: EventQueue): Process[Handle, (Event, EngineState)] =
    receive(q).evalMap(runE(q))

  def process(q: EventQueue)(qs: EngineState): Process[Task, (Event, EngineState)] =
    mapEvalState(q.dequeue, qs, runE(q))

  // Functions for type bureaucracy

  /**
    * This creates an `Event` Process with `Handle` as effect.
    */
  def receive(queue: EventQueue): Process[Handle, Event] = hoistHandle(queue.dequeue)

  def pure[A](a: A): Handle[A] = Applicative[Handle].pure(a)

  private val unit: Handle[Unit] = pure(Unit)

  val get: Handle[EngineState] =
    MonadState[Handle, EngineState].get

  private def gets[A](f: (EngineState) => A): Handle[A] =
    MonadState[Handle, EngineState].gets(f)

  private def modify(f: (EngineState) => EngineState): Handle[Unit] =
    MonadState[Handle, EngineState].modify(f)

  private def put(qs: EngineState): Handle[Unit] =
    MonadState[Handle, EngineState].put(qs)

  private def getS(id: Sequence.Id): Handle[Option[Sequence.State]] = get.map(_.get(id))

  private def getSs[A](id: Sequence.Id)(f: Sequence.State => A): Handle[Option[A]] = gets(x => x.get(id).map(f))

  private def modifyS(id: Sequence.Id)(f: Sequence.State => Sequence.State): Handle[Unit] =
    modify(st => st.get(id).map(s => st.updated(id, f(s))).getOrElse(st))

  private def putS(id: Sequence.Id)(s: Sequence.State): Handle[Unit] = modify(_.updated(id, s))

  // For introspection
  def printSequenceState(id: Sequence.Id): Handle[Option[Unit]] = getSs(id)((qs: Sequence.State) => Task.now(println(qs)).liftM[HandleStateT])

  // The `Catchable` instance of `Handle`` needs to be manually written.
  // Without it it's not possible to use `Handle` as a scalaz-stream process effects.
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
