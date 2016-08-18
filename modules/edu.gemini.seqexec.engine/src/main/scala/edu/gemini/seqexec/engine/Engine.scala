package edu.gemini.seqexec.engine

import Event._
import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.Sink
import scalaz.stream.async.mutable.{Queue => SQueue}

object Engine {

  type EventQueue = SQueue[Event]
  /**
    * Type constructor where all all side effects related to the Seqexec are
    * managed.
    */
  type Seqexec[A] = SeqexecStateT[Task, A]
  // Helper alias to facilitate lifting.
  type SeqexecStateT[M[_], A] = StateT[M, QueueStatus, A]

  /**
    * Return `SeqStatus` while changing `Status` within the `Seqexec` monad.
    * This also takes care of initiating the execution when transitioning to
    * `Running` status.
    */
  def switch(q: EventQueue)(st: Status): Seqexec[QueueStatus] =
    modify (QueueStatus.status.set(_, st)) *>
      whenM(st == Status.Running)(
        prime >>= (_.fold(unit)(step(q)(_)))
      ) *> get

  /**
    * Transfer the next pending `Step` to current `Step` when the current `Step`
    * is empty. If the current Step is not empty, `None` without any change in
    * the `SeqStatus`.
    */
  private val prime: Seqexec[Option[Execution.Current]] =
    gets(QueueStatus.prime(_)) >>= {
      case Some(qs) => put(qs) *> pure(Some(qs.queue.current))
      case None => pure(None)
    }

  /**
    * Checks the `Status` is `Running` and executes all actions in a current
    * `Step` in parallel. It also updates the `SeqStatus` as needed.
    */
  private def step(q: EventQueue)(actions: Execution.Current): Seqexec[Unit] = {

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
        ).liftM[SeqexecStateT]
      ).void
      case Status.Waiting => unit
    }
  }

  /**
    * Given the index in the current `Step` of the completed action, it
    * transfers such action to the current done Step. If the current Step
    * becomes empty it takes care of priming the next Step.
    */
  def complete(q: EventQueue)(i: Int): Seqexec[QueueStatus] =
    modify(QueueStatus.shift(i)(_)) *> (
      prime >>= (_.fold(unit)(step(q)(_)))
    ) *> get

  // For now stop the seqexec when an action fails.
  def fail(q: EventQueue)(i: Int): Seqexec[QueueStatus] = switch(q)(Status.Waiting)

  /**
    * Ask for the current `Status` within the `Seqexec` monad.
    */
  val status: Seqexec[Status] = gets(_.status)

  /**
    * Send an event within the `Seqexec` monad.
    */
  private def send(q: EventQueue)(ev: Event): Seqexec[Unit] = pure(q.enqueueOne(ev))

  /**
    * Log within the `Seqexec` monad as a side effect while returning the
    * `SeqStatus`.
    */
  // XXX: log4j?
  def log(msg: String): Seqexec[QueueStatus] = pure(println(msg)) *> get

  /**
    * Add a `Step` to the beginning of the Sequence while returning the
    * `SeqStatus`.
    */
  def add(pend: Execution.Pending): Seqexec[QueueStatus] =
    modify(qs => QueueStatus.pending.mod(pend :: _, qs)) *> get

  /** Terminates the queue while returning the final `SeqStatus`
    */
  def close(queue: EventQueue): Seqexec[QueueStatus] =
    queue.close.liftM[SeqexecStateT] *> get

  // Functions to deal with type bureaucracy

  /**
    * This creates a `Event` Process with `Seqexec` as effect.
    */
  def receive(queue: EventQueue): Process[Seqexec, Event] = hoistSeqexec(queue.dequeue)

  // Type bureaucracy

  private def pure[A](a: A): Seqexec[A] = Applicative[Seqexec].pure(a)

  private val unit: Seqexec[Unit] = pure(Unit)

  private val get: Seqexec[QueueStatus] =
    MonadState[Seqexec, QueueStatus].get

  private def gets[A](f: (QueueStatus) => A): Seqexec[A] =
    MonadState[Seqexec, QueueStatus].gets(f)

  private def modify(f: (QueueStatus) => QueueStatus) =
    MonadState[Seqexec, QueueStatus].modify(f)

  private def put(qs: QueueStatus): Seqexec[Unit] =
    MonadState[Seqexec, QueueStatus].put(qs)

  // The `Catchable` instance of `Seqexec`` needs to be manually written.
  // Without it's not possible to use `Seqexec` as a scalaz-stream process effects.
  implicit val telescopeInstance: Catchable[Seqexec] =
    new Catchable[Seqexec] {
      def attempt[A](a: Seqexec[A]): Seqexec[Throwable \/ A] = a >>= (
        x => Catchable[Task].attempt(Applicative[Task].pure(x)).liftM[SeqexecStateT]
      )
      def fail[A](err: Throwable) = Catchable[Task].fail(err).liftM[SeqexecStateT]
    }

  /**
    * Lifts from `Task` to `Seqexec` as the effect of a `Process`.
    */
  def hoistSeqexec[A](p: Process[Task, A]): Process[Seqexec, A] = {
    val toSeqexec = new (Task ~> Seqexec) {
      def apply[B](t: Task[B]): Seqexec[B] = t.liftM[SeqexecStateT]
    }
    p.translate(toSeqexec)
  }

  /**
    * Lifts from `Task` to `Seqexec` as the effect of a `Sink`.
    */
  def hoistSeqexecSink[O](s: Sink[Task, O]): Sink[Seqexec, O] =
    hoistSeqexec(s).map(_.map(_.liftM[SeqexecStateT]))
}
