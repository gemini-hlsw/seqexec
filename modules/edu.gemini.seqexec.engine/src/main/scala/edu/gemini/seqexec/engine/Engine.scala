package edu.gemini.seqexec.engine

import Event._
import State._
import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.Sink
import scalaz.stream.async.mutable.Queue

object Engine {
  /**
    * Type constructor where all all side effects related to the Telescope are
    * managed.
    */
  type Telescope[A] = TelescopeStateT[Task, A]
  // Helper alias to facilitate lifting.
  type TelescopeStateT[M[_], A] = StateT[M, SeqStatus, A]

  /**
    * Return `SeqStatus` while changing `Status` within the `Telescope` monad.
    * This also takes care of initiating the execution when transitioning to
    * `Running` status.
    */
  def switch(queue: Queue[Event])(st: Status): Telescope[SeqStatus] =
    MonadState[Telescope, SeqStatus].modify {
      case (SeqStatus(seq, st0)) => SeqStatus(seq, st)
    } *> whenM(st == Running)(
      prime >>= {
        case Some(actions) => step(queue)(actions)
        case None => unit
      }
    ) *> ask

  /**
    * Transfer the next pending `Step` to current `Step` when the current `Step`
    * is empty. If the current Step is not empty, `None` without any change in
    * the `SeqStatus`.
    */
  private val prime: Telescope[Option[StepCurrent]] =
    MonadState[Telescope, SeqStatus].gets(State.prime(_)) >>= {
      case Some(ss) => MonadState[Telescope, SeqStatus].put(ss) *>
          Applicative[Telescope].pure(Some(ss.sequence.current))
      case None => Applicative[Telescope].pure(None)
    }

  /**
    * Checks the `Status` is `Running` and executes all actions in a current
    * `Step` in parallel. It also updates the `SeqStatus` as needed.
    */
  private def step(queue: Queue[Event])(actions: StepCurrent): Telescope[Unit] = {
    // Send the expected event when action is executed
    def execute(t: (Int, Action)): Task[Unit] = {
      val (i, action) = t
      action >>= {
        case OK => queue.enqueueOne(completed(i))
        case Error => queue.enqueueOne(failed(i))
      }
    }
    status >>= {
      case Running => (
        Nondeterminism[Task].gatherUnordered(
          actions.toList.map(execute(_))
        ).liftM[TelescopeStateT]
      ).void
      case Waiting => unit
    }
  }

  /**
    * Given the index in the current `Step` of the completed action, it
    * transfers such action to the current done Step. If the current Step
    * becomes empty it takes care of priming the next Step.
    */
  def complete(queue: Queue[Event])(i: Int): Telescope[SeqStatus] = (
    MonadState[Telescope, SeqStatus].modify(shift(i)(_)) *> prime >>= {
      case Some(actions) => step(queue)(actions)
      case None => unit
    }) *> ask

  // For now stop the seqexec when an action fails.
  def fail(queue: Queue[Event])(i: Int): Telescope[SeqStatus] = switch(queue)(Waiting)

  /**
    * Ask for the current `Status` within the `Telescope` monad.
    */
  val status: Telescope[Status] =
    MonadState[Telescope, SeqStatus].gets({ case (SeqStatus(_, st)) => st })

  /**
    * Send an event within the `Telescope` monad.
    */
  private def send(queue: Queue[Event])(ev: Event): Telescope[Unit] =
    Applicative[Telescope].pure(queue.enqueueOne(ev))

  /**
    * Log within the `Telescope` monad as a side effect while returning the
    * `SeqStatus`.
    */
  // XXX: log4j?
  def log(msg: String): Telescope[SeqStatus] = Applicative[Telescope].pure(println(msg)) *> ask

  /**
    * Add a `Step` to the beginning of the Sequence while returning the
    * `SeqStatus`.
    */
  def add(ste: Step): Telescope[SeqStatus] =
    MonadState[Telescope, SeqStatus].modify(
      ss => sequenceL.andThen(pendingL).mod(ste :: _, ss)
    ) *> ask

  /**
    * Get the current State
    */
  val ask: Telescope[SeqStatus] = MonadState[Telescope, SeqStatus].get

  /** Terminates the queue while returning the final `SeqStatus`
    */
  def close(queue: Queue[Event]): Telescope[SeqStatus] =
    queue.close.liftM[TelescopeStateT] *> ask

  // Functions to deal with type bureaucracy

  /**
    * This creates a `Event` Process with `Telescope` as effect.
    */
  def receive(queue: Queue[Event]): Process[Telescope, Event] = hoistTelescope(queue.dequeue)

  private val unit: Telescope[Unit] = Applicative[Telescope].pure(Unit)

  // Type bureaucracy

  // The `Catchable` instance of `Telescope`` needs to be manually written.
  // Without it's not possible to use `Telescope` as a scalaz-stream process effects.
  implicit val telescopeInstance: Catchable[Telescope] =
    new Catchable[Telescope] {
      def attempt[A](a: Telescope[A]): Telescope[Throwable \/ A] = a >>= (
        x => Catchable[Task].attempt(Applicative[Task].pure(x)).liftM[TelescopeStateT]
      )
      def fail[A](err: Throwable) = Catchable[Task].fail(err).liftM[TelescopeStateT]
    }

  /**
    * Lifts from `Task` to `Telescope` as the effect of a `Process`.
    */
  def hoistTelescope[A](p: Process[Task, A]): Process[Telescope, A] = {
    val toTelescope = new (Task ~> Telescope) {
      def apply[B](t: Task[B]): Telescope[B] = t.liftM[TelescopeStateT]
    }
    p.translate(toTelescope)
  }

  /**
    * Lifts from `Task` to `Telescope` as the effect of a `Sink`.
    */
  def hoistTelescopeSink[O](s: Sink[Task, O]): Sink[Telescope, O] =
    hoistTelescope(s).map(_.map(_.liftM[TelescopeStateT]))
}
