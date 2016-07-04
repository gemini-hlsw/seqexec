package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

object Sequence {

  type Result = String

  sealed trait ActionResult
  case class SimpleActionResult (r: Result) extends ActionResult
  case class ComposedActionResult (r: List[ActionResult]) extends ActionResult

  sealed trait Action
  case object NullAction extends Action
  case class SimpleAction(t: Task[Result]) extends Action
  case class ParallelAction(l: List[Action]) extends Action
  case class SequentialAction(l: List[Action]) extends Action

  // Traverse the action tree and execute the tasks
  def runActions(a: Action): Task[ActionResult] = a match {
    case SimpleAction(t) => t map SimpleActionResult
    case SequentialAction(l) => Nondeterminism[Task].sequence(l map runActions).map(x => ComposedActionResult(x))
    case ParallelAction(l) => Nondeterminism[Task].gather(l map runActions).map(x => ComposedActionResult(x))
  }

  def main(args: Array[String]): Unit = {

    val step1Tcs = SimpleAction(Task {
                                  println("Start TCS configuration for Step 1")
                                  Thread.sleep(2000)
                                  println("Complete TCS configuration for Step 1")
                                  "Completed"
                                })

    val step1Ins = SimpleAction(Task {
                                  println("Start instrument configuration for Step 1")
                                  Thread.sleep(2000)
                                  println("Complete instrument configuration for Step 1")
                                  "Completed"
                                })

    val step1Obs = SimpleAction(Task {
                                  println("Start observation for Step 1")
                                  Thread.sleep(5000)
                                  println("Complete observation for Step 1")
                                  "Completed"
                                })

    val step1 = SequentialAction(List(ParallelAction(List(step1Tcs, step1Ins)), step1Obs))

    runActions(step1).unsafePerformSync
  }
}
