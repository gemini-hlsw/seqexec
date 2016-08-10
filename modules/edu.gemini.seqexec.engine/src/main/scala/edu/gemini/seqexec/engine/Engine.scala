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
    */
  def switch(queue: Queue[Event])(st: Status): Telescope[SeqStatus] =
    MonadState[Telescope, SeqStatus].modify(
      (ss: SeqStatus) => ss match {
        case (SeqStatus(seq, st0)) => SeqStatus(seq, st)
      }) *> ask

  /**
    * Checks the status is running and launches all parallel tasks to complete
    * the next step. It also updates the `Telescope` state as needed.
    */
  private def step(queue: Queue[Event]): Telescope[SeqStatus] = { // Send the result event when action is executed
    def execute(t: (Action, Int)): Task[Unit] = {
      val (action, i) = t
      action >>= {
        (r: Result) => r match {
          case OK => Task.delay { queue.enqueueOne(completed(i)) }
          case Error => Task.delay { queue.enqueueOne(failed(i)) }
        }
      }
    }

    (status >>= {
      (st: Status) => st match {
        case Running =>
          peek >>= (
            mas => mas match {
              case Some(actions) => Nondeterminism[Task].gather(
                actions.zipWithIndex.map(execute(_))).liftM[TelescopeStateT].void
              case None => send(queue)(finished)
            }
          )
        case Waiting => send(queue)(pause)
      }
     }
    ) *> ask
 }

  /**
    * Removes the first Step in the Sequence.
    * To be used after syncing.
    */
  def complete(queue: Queue[Event])(i: Int): Telescope[SeqStatus] =
    MonadState[Telescope, SeqStatus].modify(shift(i)) *> step(queue)


  def fail(i: Int): Telescope[SeqStatus] =
    MonadState[Telescope, SeqStatus].modify(shift(i)) *> ask

  /**
    * Obtain the next step in the `Sequence`. It doesn't remove the Step from
    * the Sequence. This is all done within the `Telescope` monad.
    */
  private val peek: Telescope[Option[Step]] = ask >>= (
    ss => Applicative[Telescope].pure(
      sequence.andThen(pending).get(ss).headOption)
    )

  /**
    * Ask for the current `Status` within the `Telescope` monad.
    */
  private val status: Telescope[Status] =
    MonadState[Telescope, SeqStatus].gets(
      ss => ss match {
        case (SeqStatus(_, st)) => st
      }
    )

  /**
    * Send an event within the `Telescope` monad.
    */
  private def send(queue: Queue[Event])(ev: Event): Telescope[Unit] =
    Applicative[Telescope].pure(Task.delay(queue.enqueueOne(ev)))

  /**
    * Return `SeqStatus` and log within the `Telescope` monad as a side effect
    */
  def log(msg:String): Telescope[SeqStatus] =
    // XXX: log4j?
    Applicative[Telescope].pure(println(msg)) *> ask

  /**
    * Returns `SeqStatus` and add a step to the beginning of the Sequence.
    */
  def add(ste: Step): Telescope[SeqStatus] = {
    MonadState[Telescope, SeqStatus].modify(
      ss => sequence.andThen(pending).mod(ste :: _, ss)
    ) *> ask
  }

  /**
    * Get the current State
    */
  def ask: Telescope[SeqStatus] = MonadState[Telescope, SeqStatus].get

  /** Terminates the queue returning the final `SeqStatus`
    */
  def close(queue: Queue[Event]): Telescope[SeqStatus] =
    queue.close.liftM[TelescopeStateT] *>
      MonadState[Telescope, SeqStatus].get

  // Functions to deal with type bureaucracy

  /**
    * Receive an event within the `Telescope` monad.
    */
  def receive(queue: Queue[Event]): Process[Telescope, Event] =
    hoistTelescope(queue.dequeue)

  // The `Catchable` instance of `Telescope`` needs to be manually written.
  // Without it's not possible to use `Telescope` as a scalaz-stream process effects.
  implicit val telescopeInstance: Catchable[Telescope] =
    new Catchable[Telescope] {
      def attempt[A](a: Telescope[A]): Telescope[Throwable \/ A] = a >>= (
        // `a.attempt` stackoverflows
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
