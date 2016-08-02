package edu.gemini.seqexec.engine

import Event._
import Sequence._
import scalaz._
import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.Sink
import scalaz.stream.async.mutable.Queue

object Engine {
  /**
    * Input Sequence and Status clubbed together
    *
    */
  type SeqStatus = (Sequence, Status)

  /**
    * Status of the telescope.
    *
    */
  sealed trait Status
  case object Running extends Status
  case object Waiting extends Status

  /**
    * Type constructor where all all side effects related to the Telescope are
    * managed.
    */
  type Telescope[A] = TelescopeStateT[Task, A]
  // Helper alias to facilitate lifting.
  type TelescopeStateT[M[_], A] = StateT[M, SeqStatus, A]

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
    * Checks the status is running and launches all parallel tasks to complete
    * the next step. It also updates the `Telescope` state as needed.
    */
  def run(queue: Queue[Event]): Telescope[SeqStatus] = {
    // Send the result event when action is executed
    def execute(action: Action): Action =
      action >>= {
        (r: Result) => r match {
          case Done  => Task.delay { queue.enqueueOne(completed) } *> action
          case Error => Task.delay { queue.enqueueOne(failed) } *> action
        }
      }

    status >>= {
      (st: Status) => st match {
        case Running => for {
          as <- step(queue)
          rs <- Nondeterminism[Task].gather(as.map(execute(_))).liftM[TelescopeStateT]
          _ <- if (Foldable[List].all(rs)(_ == Done)) {
               // Remove step and send Synced event
               // TODO: Change status of Action before returning `SeqStatus`
               send(queue)(synced)
               } else {
               // Just send Failed event. Because it doesn't drop the step it will
               // be reexecuted again.
               send(queue)(syncFailed)
               }
          ss <- MonadState[Telescope, SeqStatus].get
        } yield ss

        // Do nothing when status is in waiting. This will make the handler
        // block waiting for more events.
        case Waiting => MonadState[Telescope, SeqStatus].get
      }
    }
  }

  /**
    * Return `SeqStatus` while changing `Status` within the `Telescope` monad.
    */
  def switch(st: Status): Telescope[SeqStatus] =
    MonadState[Telescope, SeqStatus].modify(
      (ss: SeqStatus) => ss.rightMap(_ => st)
    ) *> MonadState[Telescope, SeqStatus].get

  /**
    * Ask for the current `Status` within the `Telescope` monad.
    */
  val status: Telescope[Status] =
    MonadState[Telescope, SeqStatus].gets(_._2)

  /**
    * Send an event within the `Telescope` monad.
    */
  def send(queue: Queue[Event])(ev: Event): Telescope[Unit] =
    Applicative[Telescope].pure(Task.delay(queue.enqueueOne(ev)))

  /**
    * Receive an event within the `Telescope` monad.
    */
  def receive(queue: Queue[Event]): Process[Telescope, Event] = {
    hoistTelescope(queue.dequeue)
  }

  /**
    * Return `SeqStatus` and log within the `Telescope` monad as a side effect
    */
  def log(msg:String): Telescope[SeqStatus] =
    // XXX: log4j?
    Applicative[Telescope].pure(println(msg)) *>
      MonadState[Telescope, SeqStatus].get

  /**
    * Obtain the next step in the `Sequence`. It doesn't remove the Step from
    * the Sequence. This is all done within the `Telescope` monad.
    */
  def step(queue: Queue[Event]): Telescope[Step] =
    MonadState[Telescope, SeqStatus].get >>= {
      ss => {
         val (seq, st) = ss
         // TODO: headDef :: a -> [a] -> a
         seq match {
           case Nil => send(queue)(finished) *> Applicative[Telescope].pure(List())
           case (x :: _) => Applicative[Telescope].pure(x)
         }
      }
    }

  /**
    * Removes the first Step in the Sequence.
    * To be used after syncing.
    */
  def tail: Telescope[Unit] =
    MonadState[Telescope, SeqStatus].modify(_.leftMap(_.tail))

  /**
    * Returns `SeqStatus` and add a step to the beginning of the Sequence.
    */
  def add(ste: Step): Telescope[SeqStatus] =
    MonadState[Telescope, SeqStatus].modify(_.leftMap(ste :: _)) *>
      MonadState[Telescope, SeqStatus].get

  /** Terminates the queue returning the final `SeqStatus`
    */
  def close(queue: Queue[Event]): Telescope[SeqStatus] = {
    queue.close.liftM[TelescopeStateT] *>
      MonadState[Telescope, SeqStatus].get
  }

  // Functions to deal with type bureaucracy

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
