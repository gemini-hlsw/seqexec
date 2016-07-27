package edu.gemini.seqexec.engine

import scala.io.StdIn.readChar
import scala.concurrent.Channel

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object Engine {

  /**
    * A List of `Step`s meant to be run sequentially.
    */
  type Sequence = List[Step]

  /**
    *  A list of actions to be run in parallel
    */
  type Step = List[Action]

  type Action = Task[Result]

  /**
    * Status of the telescope.
    *
    */
  sealed trait Status
  case object Running extends Status
  case object Waiting extends Status

  /**
    * Input Sequence and Status clubbed together
    *
    */
  type SeqStatus = (Sequence, Status)

  /**
    * The result of an action.
    *
    */
  sealed trait Result
  case object Done extends Result
  case object Error extends Result

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

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  def configureTcs: Action  = for
    { _ <- Task.delay { println("System: Start TCS configuration") }
      _ <- Task.delay { Thread.sleep(2000) }
      _ <- Task.delay { println ("System: Complete TCS configuration") }
    } yield Done

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  def configureInst : Action  = for
    { _ <- Task.delay { println("System: Start Instrument configuration") }
      _ <- Task.delay { Thread.sleep(2000) }
      _ <- Task.delay { println("System: Complete Instrument configuration") }
    } yield Done

  /**
    * Emulates an observation in the real world.
    *
    */
  def observe : Action  = for
    { _ <- Task.delay { println("System: Start observation") }
      _ <- Task.delay { Thread.sleep(2000) }
      _ <- Task.delay { println ("System: Complete observation") }
    } yield Done

  /**
    * Type constructor where all all side effects related to the Telescope are
    * managed.
    */
  type Telescope[A] = TelescopeStateT[Task, A]
  // Helper alias to facilitate lifting.
  type TelescopeStateT[M[_], A] = StateT[M, SeqStatus, A]

  /**
    * Main logical thread to handle events and produce output.
    */
  def handler[R](chan: Channel[Event]): Telescope[R] =
    Bind[Telescope].forever(
      receive(chan) >>= {
        (ev: Event) => ev match {
          case Start => log("Output: Started") *> switch(Running) *> run(chan)
          case Pause => log("Output: Paused") *> switch(Waiting)
          case Completed => log("Output: Action completed")
          case Failed => log("Output: Action failed")
          case Synced => log("Output: Parallel actions completed") *> run(chan)
          case SyncFailed => log("Output: Step failed. Repeating...") *> run(chan)
          case Finished => log("Output: Finished") *> switch(Waiting)
        }
      }
    )


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
            tail *> send(chan)(Synced)
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

  // /**
  //   * Handles console input.
  //   * TODO: Doesn't work as it is because of console input buffering.
  //   */
  // def input[A](chan: Channel[Event]): Task[A] =
  //   Bind[Task].forever(
  //     Task { readChar() } >>= {
  //       (c: Char) => c match {
  //         case 'p' => Task { chan.write(Pause) }
  //         case 's' => Task { chan.write(Start) }
  //         case _   => Task(Unit)
  //       }
  //     }
  //)
}
