package edu.gemini.seqexec.engine

import scala.io.StdIn.readChar
import scala.concurrent.Channel

import scalaz._
import Scalaz._
import scalaz.concurrent.Task


object Engine {

  type Sequence = List[List[Action]]

  type Action = Task[Result]

  sealed trait Status
  case object Running extends Status
  case object Waiting extends Status

  type SeqStatus = (Sequence, Status)

  sealed trait Result
  case object Done extends Result
  case object Error extends Result

  sealed trait Event
  case object Start extends Event
  case object Pause extends Event
  case object Completed extends Event  // Action failed
  case object Failed extends Event // Action failed
  case object Synced extends Event // Parallel actions completed
  case object Finished extends Event

  def configureTcs: Action  = for
    { _ <- Task { println("Start TCS configuration") }
      _ <- Task { Thread.sleep(2000) }
      _ <- Task { println ("Complete TCS configuration") }
    } yield Done

  def configureInst : Action  = for
    { _ <- Task { println("Start Instrument configuration") }
      _ <- Task { Thread.sleep(2000) }
      _ <- Task { println ("Complete Instrument configuration for") }
    } yield Done

  def observe : Action  = for
    { _ <- Task { println("Start observation") }
      _ <- Task { Thread.sleep(2000) }
      _ <- Task { println ("Complete observation") }
    } yield Done

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

  type Telescope[A] = ReaderWriterStateT[Task, Channel[Event], List[String], SeqStatus, A]

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

  def log(msg:String): Telescope[Unit] =
    MonadTell[Telescope, List[String]].tell(List(msg))

  val receive: Telescope[Event] = for {
    chan <- channel
  } yield chan.read

  def channel: Telescope[Channel[Event]] =
    MonadReader[Telescope, Channel[Event]].ask

  def switch(st: Status): Telescope[Unit] =
    MonadState[Telescope, SeqStatus].modify(
      (ss: SeqStatus) => ss.map(_ => st)
    )

  val status: Telescope[Status] =
    MonadState[Telescope, SeqStatus].gets(_._2)

  val execute: Telescope[Unit] = {
    def exe(chan: Channel[Event], action:Action): Task[Unit] =
      action >>= {
        (r: Result) => r match {
          case Done  => Task { chan.write(Completed) }
          case Error => Task { chan.write(Failed) }
        }
      }

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
        case Waiting => Applicative[Telescope].pure(Unit)
      }
    }
  }

  def send(ev: Event): Telescope[Unit] = for {
    chan <- channel
  } yield Applicative[Telescope].pure(
    Task { chan.write(ev) }
  )

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

  val sequence0: Sequence =
    List(
      List(configureTcs, configureInst),
      List(observe),
      List(configureTcs, configureInst),
      List(observe)
    )

  def main(args: Array[String]): Unit = {
    val chan = new Channel[Event]
    /// XXX: Handle printing
    val ((output, _), _) = Nondeterminism[Task].both(
      handler.exec(chan, (sequence0, Waiting)),
      input(chan)
    ).unsafePerformSync
  }
}
