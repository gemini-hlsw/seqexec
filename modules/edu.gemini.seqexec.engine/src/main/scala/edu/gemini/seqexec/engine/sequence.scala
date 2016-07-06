package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import effect.IO
import IO.putStrLn

object Engine {

  type Sequence = List[Step]

  sealed trait Result
  case object Done  extends Result
  case object Error extends Result

  case class Step (t: TcsConfig, i: InstConfig, o: Observation)

  // case class TcsConfig (r: IO[Result])
  // Haskell's newtype, how to extract inner IO easily?
  type TcsConfig  = IO[Result]
  type InstConfig = IO[Result]
  type Observation= IO[Result]

  def execute(seq: Sequence): IO[Unit] = {

    def step(s:Step): IO[Unit] = s match {
      case Step(tcs: IO[Result], inst: IO[Result], obsv: IO[Result]) => for {
        r <- concurrently(tcs, inst)
        (tr, ir) = r
        obr <- obsv
        } yield ()
      }
    seq.traverse_(step)
  }

  private def concurrently[A,B](a: IO[A], b: IO[B]): IO[(Result,Result)] = ???

  // TODO: Can't this be made less ugly?
  val sequence1 = List(Step(
      (for { _ <- putStrLn("Start TCS configuration for Step 1")
             _ <- IO(Thread.sleep(2000))
             _ <- putStrLn("Complete TCS configuration for Step 1")
             } yield Done
      ),
      (for { _ <- putStrLn("Start instrument configuration for Step 1")
             _ <- IO(Thread.sleep(2000))
             _ <- putStrLn("Complete instrument configuration for Step 1")
                } yield Done
      ),
      (for  { _ <- putStrLn("Start observation for Step 1")
              _ <- IO(Thread.sleep(5000))
              _ <- putStrLn("Complete observation for Step 1")
              } yield Done
      )))

  def main(args: Array[String]): Unit = execute(sequence1).unsafePerformIO()

}
