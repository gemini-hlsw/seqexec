package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

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

  def execute(seq: Sequence): Task[Unit] = {

    def step(s:Step): Task[Unit] = s match {
      case Step(tcs: TcsConfig, inst: InstConfig, obsv: Observation) => for {
        r <- concurrently(action(tcs), action(inst))
        (tr, ir) = r
        obr <- action(obsv)
        } yield ()
      }
    seq.traverse_(step)
  }

  private def concurrently[A,B](a: Task[A], b: Task[B]): Task[(A, B)] = ???
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
  def main(args: Array[String]): Unit = execute(sequence1).unsafePerformSync

}
