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
  type Sequence = List[Actions]

  type Actions = List[Action]

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
  case object Failed extends Event
  // when a set of parallel actions is completed.
  case object Synced extends Event
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
      _ <- Task { println ("Complete Instrument configuration for") }
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
           case 'p' => Task { chan.write(Pause); println("hola") }
           case 's' => Task { chan.write(Start) }
           case _   => Task(Unit)
         }
        }
    )

  /**
    * Monad on which to shove all effects ever needed.
    */
  type Telescope[A] = ReaderWriterStateT[Task, Channel[Event], List[String], SeqStatus, A]

  /**
    * Main logical thread to handle events and produce output.
    */
  def handler[R]: Telescope[R] =
    Bind[Telescope].forever(
      receive >>= {
        (ev: Event) => ev match {
          case Start => log("Output: Started") *> switch(Running) *> execute
          case Pause => log("Output: Paused") *> switch(Waiting)
          case Completed => log("Output: Action completed")
          case Failed => log("Output: Action failed")
          case Synced => log("Output: Parallel actions completed") *> execute
          case Finished => log("Output: Finished") *> switch(Waiting)
        }
      }
    )

  /**
    * Log within the `Telescope` monad.
    */
  def log(msg:String): Telescope[Unit] =
    MonadTell[Telescope, List[String]].tell(List(msg))

  /**
    * Receive an event within the `Telescope` monad.
    */
  val receive: Telescope[Event] = for {
    chan <- channel
  } yield chan.read

  /**
    * Receive an event within the `Telescope` monad.
    */
  def channel: Telescope[Channel[Event]] =
    MonadReader[Telescope, Channel[Event]].ask

  /**
    * Change Status within the `Telescope` monad.
    */
  def switch(st: Status): Telescope[Unit] =
    MonadState[Telescope, SeqStatus].modify(
      (ss: SeqStatus) => ss.map(_ => st)
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
  val execute: Telescope[Unit] = {
    // Execute just one task.
    def exe(chan: Channel[Event], action:Action): Task[Unit] =
      action >>= {
        (r: Result) => r match {
          case Done  => Task { chan.write(Completed) }
          case Error => Task { chan.write(Failed) }
        }
      }

    // Helper function to facilitate recursion.
    val go: Telescope[Unit] = for {
      actions <- pop
      chan <- MonadReader[Telescope, Channel[Event]].ask
      _ <- Applicative[Telescope].pure(
        Nondeterminism[Task].gatherUnordered(
          actions.map(
            (a: Action) => exe(chan, a)
          )
        )
      )
    } yield send(Synced)

    status >>= {
      (st: Status) => st match {
        case Running => go
        // Do nothing when status is in waiting. This will make the handler
        // block waiting for more events.
        case Waiting => Applicative[Telescope].pure(Unit)
      }
    }
  }

  /**
    * Send an event within the `Telescope` monad.
    */
  def send(ev: Event): Telescope[Unit] = for {
    chan <- channel
  } yield Applicative[Telescope].pure(
    Task { chan.write(ev) }
  )

  /**
    * Obtain the next step in the `Sequence` and update the `Telescope` state accordingly.
    * This is all done within the `Telescope` monad.
    */
  def pop: Telescope[List[Action]] =
    MonadState[Telescope, SeqStatus].get >>= {
      (ss) => {
         val (seq, st) = ss
         seq match {
           case (x :: xs) => MonadState[Telescope, SeqStatus].put((xs,st)) *> Applicative[Telescope].pure(x)
           case _ => send(Finished) *> Applicative[Telescope].pure(List())
         }
      }
    }

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
      handler.exec(chan, (sequence0, Waiting)),
      input(chan)
    ).unsafePerformSync
  }
}
