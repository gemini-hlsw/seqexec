package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Event._
import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.Sink
import scalaz.stream.async.mutable.{Queue => SQueue}

package object engine {

  type EventQueue = SQueue[Event]
  /**
    * Type constructor where all all side effects related to the Engine are
    * managed.
    */
  type Engine[A] = EngineStateT[Task, A]
  // Helper alias to facilitate lifting.
  type EngineStateT[M[_], A] = StateT[M, QueueStatus, A]

  /**
    * Return `SeqStatus` while changing `Status` within the `Engine` monad.
    * This also takes care of initiating the execution when transitioning to
    * `Running` status.
    */
  def switch(q: EventQueue)(st: Status): Engine[QueueStatus] =
    modify (QueueStatus.status.set(_, st)) *>
      whenM(st == Status.Running)(
        prime >>= (_.fold(unit)(step(q)(_)))
      ) *> get

  /**
    * Transfer the next pending `Step` to current `Step` when the current `Step`
    * is empty. If the current Step is not empty, `None` without any change in
    * the `SeqStatus`.
    */
  private val prime: Engine[Option[Execution.Current]] =
    gets(QueueStatus.prime(_)) >>= {
      case Some(qs) => put(qs) *> pure(Some(qs.queue.current))
      case None => pure(None)
    }

  /**
    * Checks the `Status` is `Running` and executes all actions in a current
    * `Step` in parallel. It also updates the `SeqStatus` as needed.
    */
  private def step(q: EventQueue)(actions: Execution.Current): Engine[Unit] = {

    // Send the expected event when action is executed
    def execute(t: (Int, Execution.Action)): Task[Unit] = {
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
      ).void
      case Status.Waiting => unit
    }
  }

  /**
    * Given the index in the current `Step` of the completed action, it
    * transfers such action to the current done Step. If the current Step
    * becomes empty it takes care of priming the next Step.
    */
  def complete(q: EventQueue)(i: Int): Engine[QueueStatus] =
    modify(QueueStatus.shift(i)(_)) *> (
      prime >>= (_.fold(unit)(step(q)(_)))
    ) *> get

  // For now stop the seqexec when an action fails.
  def fail(q: EventQueue)(i: Int): Engine[QueueStatus] = switch(q)(Status.Waiting)

  /**
    * Ask for the current `Status` within the `Engine` monad.
    */
  val status: Engine[Status] = gets(_.status)

  /**
    * Send an event within the `Engine` monad.
    */
  private def send(q: EventQueue)(ev: Event): Engine[Unit] = pure(q.enqueueOne(ev))

  /**
    * Log within the `Engine` monad as a side effect while returning the
    * `SeqStatus`.
    */
  // XXX: log4j?
  def log(msg: String): Engine[QueueStatus] = pure(println(msg)) *> get

  /**
    * Add a `Step` to the beginning of the Sequence while returning the
    * `SeqStatus`.
    */
  def add(pend: Execution.Pending): Engine[QueueStatus] =
    modify(qs => QueueStatus.pending.mod(pend :: _, qs)) *> get

  /** Terminates the queue while returning the final `SeqStatus`
    */
  def close(queue: EventQueue): Engine[QueueStatus] =
    queue.close.liftM[EngineStateT] *> get

  // Functions to deal with type bureaucracy

  /**
    * This creates a `Event` Process with `Engine` as effect.
    */
  def receive(queue: EventQueue): Process[Engine, Event] = hoistEngine(queue.dequeue)

  // Type bureaucracy

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
