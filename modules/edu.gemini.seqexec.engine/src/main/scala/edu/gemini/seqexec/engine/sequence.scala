package edu.gemini.seqexec.engine

import scala.io.StdIn.readChar
import scala.concurrent.Channel

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object Engine {

  /**
    * Each `Actions` in the first level list is meant to be run sequentially.
    * Each `Action` within the second level of the list is meant to be run in
    * parallel.
    */
  type Sequence = List[Step]

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
    { _ <- Task { println("Start TCS configuration") }
      _ <- Task { Thread.sleep(2000) }
      _ <- Task { println ("Complete TCS configuration") }
    } yield Done

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  def configureInst : Action  = for
    { _ <- Task { println("Start Instrument configuration") }
      _ <- Task { Thread.sleep(2000) }
      _ <- Task { println("Complete Instrument configuration for") }
    } yield Done

  /**
    * Emulates an observation in the real world.
    *
    */
  def observe : Action  = for
    { _ <- Task { println("Start observation") }
      _ <- Task { Thread.sleep(2000) }
      _ <- Task { println ("Complete observation") }
    } yield Done

  /**
    * Handles console input.
    * TODO: Doesn't work as it is because of console input buffering.
    */
  def input[A](chan: Channel[Event]): Task[A] =
    Bind[Task].forever(
      Task { readChar() } >>= {
        (c: Char) => c match {
           case 'p' => Task { chan.write(Pause) }
           case 's' => Task { chan.write(Start) }
           case _   => Task(Unit)
         }
        }
    )

  /**
    *
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
          case Start => log("Output: Started") *> switch(Running) *> execute(chan)
          case Pause => log("Output: Paused") *> switch(Waiting)
          case Completed => log("Output: Action completed")
          case Failed => log("Output: Action failed")
          case Synced => log("Output: Parallel actions completed") *> execute(chan)
          case SyncFailed => log("Output: Step failed. Repeating...") *> execute(chan)
          case Finished => log("Output: Finished") *> switch(Waiting)
        }
      }
    )

  /**
    * Log within the `Telescope` monad.
    */
  def log(msg:String): Telescope[Unit] =
    // XXX: Replace with equivalent of Writer monad.
    Applicative[Telescope].pure(println(msg))

  /**
    * Receive an event within the `Telescope` monad.
    */
  def receive(chan: Channel[Event]): Telescope[Event] =
    Applicative[Telescope].pure{ chan.read }

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
    * Checks the status is running and launches all parallel tasks to complete
    * the next step. It also updates the `Telescope` state as needed.
    */
  def execute(chan: Channel[Event]): Telescope[Unit] = {
    // Send the result event when action is executed
    def exe(action: Action): Action =
      action >>= {
        (r: Result) => r match {
          case Done  => Task { chan.write(Completed) } *> action
          case Error => Task { chan.write(Failed) } *> action
        }
      }

    status >>= {
      (st: Status) => st match {
        case Running => for {
          as <- step(chan)
          rs <- Nondeterminism[Task].gather(as.map(exe(_))).liftM[TelescopeStateT]
        } yield {
          if (Foldable[List].all(rs)(_ == Done)) {
            // Remove step and send Synced event
            drop *> send(chan)(Synced)
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
    * Send an event within the `Telescope` monad.
    */
  def send(chan: Channel[Event])(ev: Event): Telescope[Unit] =
    Applicative[Telescope].pure(Task { chan.write(ev) })

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
           case (x :: _) => Applicative[Telescope].pure(x)
           case _ => send(chan)(Finished) *> Applicative[Telescope].pure(List())
         }
      }
    }

  /**
    * Drops the first Step in the Sequence.
    * To be used after syncing.
    */
  def drop: Telescope[Unit] =
    MonadState[Telescope, SeqStatus].modify(
      (ss: SeqStatus) => ss.leftMap(_.tail)
    )
  // Dummy sequence.
  val sequence0: Sequence =
    List(
      List(configureTcs, configureInst),
      List(observe),
      List(configureTcs, configureInst),
      List(observe)
    )

  def main(args: Array[String]): Unit = {
    val chan = new Channel[Event]
    // This spawns the input thread and the handler thread with its initial
    // state and the event // channel
    val ((output, _), _) = Nondeterminism[Task].both(
      handler(chan).exec((sequence0, Waiting)),
      input(chan)
    ).unsafePerformSync
  }
}
