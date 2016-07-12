package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scala.concurrent.Channel


object Engine {

  type Sequence = List[Step]

  type Action = Task[Result]

  sealed trait Result
  case object Done  extends Result
  case object Error extends Result

  case class Step(t: TcsConfig, i: InstConfig, o: Observation)

  sealed trait HasAction
  case class TcsConfig(a:Action) extends HasAction
  case class InstConfig(a:Action) extends HasAction
  case class Observation(a:Action) extends HasAction

  def action(ha:HasAction): Action = ha match {
    case TcsConfig(a:Action) => a
    case InstConfig(a:Action) => a
    case Observation(a:Action) => a
  }

  sealed trait Event
  case object Configured extends Event
  case object Observed   extends Event
  case object Completed  extends Event

  def execute(chan: Channel[Event], seq: Sequence): Task[Unit] = {
    def step(s:Step): Task[Unit] = s match {
      case Step(tcs: TcsConfig, inst: InstConfig, obsv: Observation) => for {
        r <- concurrently(action(tcs), action(inst))
        _ <- Task { if (r.equals((Done, Done))) { chan.write(Configured) } }
        obr <- action(obsv)
        } yield chan.write(Completed)
      }
    seq.traverse_(step)
  }

  sealed trait Status
  case object Idle extends Status
  case object Running extends Status
  case object Failed extends Status

  def handle (chan: Channel[Event]): Task[Unit] = {
    def go (s:Status): Task[Unit] = s match {
      case Idle => Task {
        println("Output: Starting sequence")
        go(Running)
      }
      case Failed => sys.error("Unimplemented")
      case Running => for {
        ev <- Task { chan.read }
        _ <- ev match {
          case Configured => Task {
            println("Output: TCS and Instrument configured")
            go(Running)
          }
          case Observed => Task {
            println("Output: Observation completed")
            go(Running)
          }
          case Completed  => Task {
            println("Output: Sequence completed")
          }
        }
      } yield Task(Unit)
    }
    go(Idle)
  }

  val sequence1 = {
    List(
      Step(
        (TcsConfig
           (for
            { _ <- Task { println("Start TCS configuration for Step 1") }
              _ <- Task { Thread.sleep(2000) }
              _ <- Task { println ("Complete TCS configuration for Step 1") }
            } yield Done
           )
        ),
        (InstConfig
           (for
            { _ <- Task { println("Start instrument configuration for Step 1") }
              _ <- Task { Thread.sleep(2000) }
              _ <- Task { println("Complete instrument configuration for Step 1") }
            } yield Done
           )
        ),
        (Observation
           (for
            { _ <- Task { println("Start observation for Step 1") }
              _ <- Task { Thread.sleep(5000) }
              _ <- Task { println("Complete observation for Step 1") }
            } yield Done
           )
        )
      )
    )
}
  def main(args: Array[String]): Unit = {
    val chan = new Channel[Event]
    concurrently(execute(chan, sequence1), handle(chan)).unsafePerformSync
  }
  private def concurrently[A, B](a: Task[A], b: Task[B]): Task[(A, B)] =
    Nondeterminism[Task].both(a, b)
}
