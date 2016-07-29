package edu.gemini.seqexec.engine

import scala.concurrent.Channel
import scalaz.concurrent.Task

import scalaz._
import Scalaz._
import edu.gemini.seqexec.engine.Sequence._

object Engine {
  /**
    * Anything that can go through the Event channel.
    *
    */
  sealed trait Event
  // Starting and resuming.
  case object Start extends Event
  case object Pause extends Event
  // when an action is completed even if it belongs to a set of
  // parallel actions.
  case object Completed extends Event
  // when an action failed
  case object Failed extends Event
  // when a set of parallel actions is completed.
  case object Synced extends Event
  case object SyncFailed extends Event
  case object Finished extends Event
  case class AddStep(a: Step) extends Event

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

  /**
    * Checks the status is running and launches all parallel tasks to complete
    * the next step. It also updates the `Telescope` state as needed.
    */
  def run(chan: Channel[Event]): Telescope[Unit] = {
    // Send the result event when action is executed
    def execute(action: Action): Action =
      action >>= {
        (r: Result) => r match {
          case Done  => Task.delay { chan.write(Completed) } *> action
          case Error => Task.delay { chan.write(Failed) } *> action
        }
      }

    status >>= {
      (st: Status) => st match {
        case Running => for {
          as <- step(chan)
          rs <- Nondeterminism[Task].gather(as.map(execute(_))).liftM[TelescopeStateT]
        } yield {
          if (Foldable[List].all(rs)(_ == Done)) {
            // Remove step and send Synced event
            send(chan)(Synced)
          } else {
            // Just send Failed event. Because it doesn't drop the step it will
            // be reexecuted again.
            send(chan)(SyncFailed)
          }
        }

        // Do nothing when status is in waiting. This will make the handler
        // block waiting for more events.
        case Waiting => Applicative[Telescope].pure(Unit)
      }
    }
  }

  /**
    * Change Status within the `Telescope` monad.
    */
  def switch(st: Status): Telescope[Unit] =
    MonadState[Telescope, SeqStatus].modify(
      (ss: SeqStatus) => ss.rightMap(_ => st)
    )

  /**
    * Ask for the current `Status` within the `Telescope` monad.
    */
  val status: Telescope[Status] =
    MonadState[Telescope, SeqStatus].gets(_._2)

  /**
    * Send an event within the `Telescope` monad.
    */
  def send(chan: Channel[Event])(ev: Event): Telescope[Unit] =
    Applicative[Telescope].pure(Task.delay { chan.write(ev) })

  /**
    * Receive an event within the `Telescope` monad.
    */
  def receive(chan: Channel[Event]): Telescope[Event] =
    Applicative[Telescope].pure{ chan.read }

  /**
    * Log within the `Telescope` monad.
    */
  def log(msg:String): Telescope[Unit] =
    // XXX: Replace with equivalent of Writer monad.
    Applicative[Telescope].pure(println(msg))

  /**
    * Obtain the next step in the `Sequence`. It doesn't remove the Step from
    * the Sequence. This is all done within the `Telescope` monad.
    */
  def step(chan: Channel[Event]): Telescope[Step] =
    MonadState[Telescope, SeqStatus].get >>= {
      ss => {
         val (seq, st) = ss
         // TODO: headDef :: a -> [a] -> a
         seq match {
           case Nil => send(chan)(Finished) *> Applicative[Telescope].pure(List())
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

  def add(ste: Step): Telescope[Unit] =
    MonadState[Telescope, SeqStatus].modify(_.leftMap(ste :: _))
}
