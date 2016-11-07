package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import org.scalatest._
import scalaz.concurrent.Task

class StepZSpec extends FlatSpec with Matchers {
  val action: Action = Task(Result.OK(Unit))
  val step0: Step[Action] = Step(1, List(Nil))
  val step1: Step[Action] = Step(1, List(List(action)))
  val step2: Step[Action] = Step(2, List(List(action, action), List(action)))

  "currentify" should "be None only when a Step is empty of executions" in {
    assert(StepZ.currentify(Step(0, Nil)).isEmpty)
    assert(StepZ.currentify(step0).isEmpty)
    assert(StepZ.currentify(step1).nonEmpty)
    assert(StepZ.currentify(step2).nonEmpty)
  }
}
