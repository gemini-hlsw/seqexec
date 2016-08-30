package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Event._
import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.Sink
import scalaz.stream.async.mutable.{Queue => SQueue}

package object engine {

  /**
    * This represents an actual real-world action to be done in the underlying
    * systems.
    */
  type Action = Task[Result]

  // Avoid name class with the proper Seqexec `Queue`
  type EventQueue = SQueue[Event]

  /**
    * Type constructor where all Seqexec side effect are managed.
    */
  type Engine[A] = EngineStateT[Task, A]
  // Helper alias to facilitate lifting.
  type EngineStateT[M[_], A] = StateT[M, QueueStatus, A]

  /**
    * Changes the `Status` and returns the new `QueueStatus`
    *
    * This also takes care of initiating the execution when transitioning to
    * `Running` status.
    */
  def switch(q: EventQueue)(st: Status): Engine[QueueStatus] =
    modify (QueueStatus.status.set(_, st)) *>
      whenM(st == Status.Running)(
        prime >>= (_.fold(unit)(step(q)(_)))
      ) *> get

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new `QueueStatus`.
    *
    * When the index doesn't exit it does nothing.
    */
  def complete(q: EventQueue)(i: Int): Engine[QueueStatus] =
    modify(QueueStatus.shift(i)(_)) *> get

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `QueueStatus`. In the future this function should handle the failed
    * action.
    */
  def fail(q: EventQueue)(i: Int): Engine[QueueStatus] = switch(q)(Status.Waiting)

  /**
    * Launches the next `Execution` when the current `Execution` is empty while
    * returning the `QueueStatus` just before the next execution starts. This is
    * mainly meant to be used to handle `Executed` events.
    */
  def next(q: EventQueue): Engine[QueueStatus] =
    (prime >>= (_.fold(unit)(step(q)(_)))) *> get

  /**
    * Adds an `Execution` to the beginning of the `QueueStatus` while returning
    * the `QueueStatus`.
    */
  // TODO: Change this to the end or insert by index. For that List -> Vector in
  // `QueueStatus`
  def add(pend: Execution.Pending): Engine[QueueStatus] = {
    val lens = QueueStatus.pending.partial >=>
      PLens.listHeadPLens[Sequence.Pending] >=>
      PLens.listHeadPLens[Step.Pending]
    modify(lens.mod((pend :: _), _)) *> get
  }

  /**
    * Ask for the current Engine `Status`.
    */
  val status: Engine[Status] = gets(_.status)

  /**
    * Log something and return the `QueueStatus`
    */
  // XXX: Proper Java logging
  def log(msg: String): Engine[QueueStatus] = pure(println(msg)) *> get

  /** Terminates the `Engine` returning the final `QueueStatus`.
    */
  def close(queue: EventQueue): Engine[QueueStatus] =
    queue.close.liftM[EngineStateT] *> get

  /**
    * Enqueue `Event` in the Engine.
    */
  private def send(q: EventQueue)(ev: Event): Engine[Unit] = pure(q.enqueueOne(ev))

  /**
    * Checks the `Status` is `Running` and executes all actions in the current
    * `Execution` in parallel. It also updates the `QueueStatus` as needed.
    */
  private def step(q: EventQueue)(actions: Execution.Current): Engine[Unit] = {

    // Send the expected event when action is executed
    def execute(t: (Int, Action)): Task[Unit] = {
      val (i, action) = t
      action >>= {
        case Result.OK => q.enqueueOne(completed(i))
        case Result.Error => q.enqueueOne(failed(i))
      }
    }
    status >>= {
      case Status.Running => (
        Nondeterminism[Task].gatherUnordered(
          actions.toList.map(execute(_))
        ).liftM[EngineStateT]
      ) *> send(q)(executed)
      case Status.Waiting => unit
    }
  }

  /**
    * Promote the next pending `Execution` to current when the current
    * `Execution` is empty. If the current `Execution` is not empty it does
    * nothing.
    */
  private val prime: Engine[Option[Execution.Current]] =
    gets(QueueStatus.prime(_)) >>= {
      case Some(qs) => put(qs) *> pure(Some(qs.queue.current))
      case None => pure(None)
    }

  // Functions to facilitate type bureaucracy

  /**
    * This creates a `Event` Process with `Engine` as effect.
    */
  def receive(queue: EventQueue): Process[Engine, Event] = hoistEngine(queue.dequeue)

  private def pure[A](a: A): Engine[A] = Applicative[Engine].pure(a)

  private val unit: Engine[Unit] = pure(Unit)

  private val get: Engine[QueueStatus] =
    MonadState[Engine, QueueStatus].get

  private def gets[A](f: (QueueStatus) => A): Engine[A] =
    MonadState[Engine, QueueStatus].gets(f)

  private def modify(f: (QueueStatus) => QueueStatus) =
    MonadState[Engine, QueueStatus].modify(f)

  private def put(qs: QueueStatus): Engine[Unit] =
    MonadState[Engine, QueueStatus].put(qs)

  // The `Catchable` instance of `Engine`` needs to be manually written.
  // Without it's not possible to use `Engine` as a scalaz-stream process effects.
  implicit val telescopeInstance: Catchable[Engine] =
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
