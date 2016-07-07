package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import effect.IO
import IO.putStrLn

object Engine {

  type Sequence = List[Step]

  type Action = IO[Result]

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

  def execute(seq: Sequence): IO[Unit] = {

    def step(s:Step): IO[Unit] = s match {
      case Step(tcs: TcsConfig, inst: InstConfig, obsv: Observation) => for {
        r <- concurrently(action(tcs), action(inst))
        (tr, ir) = r
        obr <- action(obsv)
        } yield ()
      }
    seq.traverse_(step)
  }

  private def concurrently[A,B](a: IO[A], b: IO[B]): IO[(A, B)] = ???

  val sequence1 = {
    List(
      Step(
        (TcsConfig
           (for
            { _ <- putStrLn("Start TCS configuration for Step 1")
              _ <- IO(Thread.sleep(2000))
              _ <- putStrLn("Complete TCS configuration for Step 1")
            } yield Done
           )
        ),
        (InstConfig
           (for
            { _ <- putStrLn("Start instrument configuration for Step 1")
              _ <- IO(Thread.sleep(2000))
              _ <- putStrLn("Complete instrument configuration for Step 1")
            } yield Done
           )
        ),
        (Observation
           (for
            { _ <- putStrLn("Start observation for Step 1")
              _ <- IO(Thread.sleep(5000))
              _ <- putStrLn("Complete observation for Step 1")
            } yield Done
           )
        )
      )
    )
}
  def main(args: Array[String]): Unit = execute(sequence1).unsafePerformIO()

}
