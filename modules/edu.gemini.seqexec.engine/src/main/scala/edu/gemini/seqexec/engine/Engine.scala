package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.async.mutable.Queue
import edu.gemini.seqexec.engine.Sequence._

object Engine {
  /**
    * Anything that can go through the Event Queue.
    *
    */
  sealed trait Event
  case class EventUser(ue: UserEvent) extends Event
  case class EventSystem(se: SystemEvent) extends Event

  sealed trait UserEvent
  case object Start extends UserEvent
  case object Pause extends UserEvent
  case class AddStep(a: Step) extends UserEvent

  val start: Event = EventUser(Start)
  val pause: Event = EventUser(Pause)
  def addStep(ste: Step): Event = EventUser(AddStep(ste))

  sealed trait SystemEvent
  // when an action is completed even if it belongs to a set of
  // parallel actions.
  case object Completed extends SystemEvent
  // when an action failed
  case object Failed extends SystemEvent
  // when a set of parallel actions is completed.
  case object Synced extends SystemEvent
  case object SyncFailed extends SystemEvent
  case object Finished extends SystemEvent

  val completed: Event = EventSystem(Completed)
  val failed: Event = EventSystem(Failed)
  val synced: Event = EventSystem(Synced)
  val syncFailed: Event = EventSystem(SyncFailed)
  val finished: Event = EventSystem(Finished)

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
  def run(queue: Queue[Event]): Telescope[Unit] = {
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
        } yield {
          if (Foldable[List].all(rs)(_ == Done)) {
            // Remove step and send Synced event
            send(queue)(synced)
          } else {
            // Just send Failed event. Because it doesn't drop the step it will
            // be reexecuted again.
            send(queue)(syncFailed)
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
  def send(queue: Queue[Event])(ev: Event): Telescope[Unit] =
    Applicative[Telescope].pure(Task.delay { queue.enqueueOne(ev) })

  /**
    * Receive an event within the `Telescope` monad.
    */
  def receive(queue: Queue[Event]): Process[Telescope, Event] = ???

  /**
    * Log within the `Telescope` monad.
    */
  def log(msg:String): Telescope[Unit] =
    // XXX: log4j?
    Applicative[Telescope].pure(println(msg))

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
    * Adds a step to the beginning of the Sequence.
    */
  def add(ste: Step): Telescope[Unit] =
    MonadState[Telescope, SeqStatus].modify(_.leftMap(ste :: _))
}
