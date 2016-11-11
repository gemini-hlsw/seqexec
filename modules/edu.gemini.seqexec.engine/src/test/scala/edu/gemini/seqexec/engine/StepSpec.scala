package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import org.scalatest.FlatSpec
import scalaz.concurrent.Task

/**
  * Created by jluhrs on 9/29/16.
  */
class StepSpec extends FlatSpec {

  // All tests check the output of running a step against the expected sequence of updates.

  // This test must have a simple step definition and the known sequence of updates that running that step creates.
  // The test will just run step and compare the output with the predefined sequence of updates.
  ignore should "run and generate the predicted sequence of updates." in {

  }

  // The difficult part is to set the pause command to interrupts the step execution in the middle.
  ignore should "stop execution in response to a pause command" in {

  }

  // It should reuse code from the previous test to set initial state.
  ignore should "resume execution from the non-running state in response to a resume command." in {

  }

  ignore should "ignore pause command if step is not been executed." in {

  }

  // Be careful that resume command really arrives while sequence is running.
  ignore should "ignore resume command if step is already running." in {

  }

  // For this test, one of the actions in the step must produce an error as result.
  ignore should "stop execution and propagate error when an Action ends in error." in {

  }

  val result = Result.OK(Unit)
  val action: Action = Task(result)
  val stepz0: StepZ  = StepZ(0, Nil, Current.empty, Nil)
  val stepza0: StepZ = StepZ(1, List(List(action)), Current.empty, Nil)
  val stepza1: StepZ = StepZ(1, List(List(action)), Current(List(result.right)), Nil)
  val stepzr0: StepZ = StepZ(1, Nil, Current.empty, List(List(result)))
  val stepzr1: StepZ = StepZ(1, Nil, Current(List(result.right, result.right)), Nil)
  val stepzr2: StepZ = StepZ(1, Nil, Current(List(result.right, result.right)), List(List(result)))
  val stepzar0: StepZ = StepZ(1, Nil, Current(List(result.right, action.left)), Nil)
  val stepzar1: StepZ = StepZ(1, List(List(action)), Current(List(result.right, result.right)), List(List(result)))

  "uncurrentify" should "be None when not all executions are completed" in {
    assert(stepz0.uncurrentify.isEmpty)
    assert(stepza0.uncurrentify.isEmpty)
    assert(stepza1.uncurrentify.isEmpty)
    assert(stepzr0.uncurrentify.isEmpty)
    assert(stepzr1.uncurrentify.nonEmpty)
    assert(stepzr2.uncurrentify.nonEmpty)
    assert(stepzar0.uncurrentify.isEmpty)
    assert(stepzar1.uncurrentify.isEmpty)
  }

  "next" should "be None when there are no more pending executions" in {
    assert(stepz0.next.isEmpty)
    assert(stepza0.next.isEmpty)
    assert(stepza1.next.nonEmpty)
    assert(stepzr0.next.isEmpty)
    assert(stepzr1.next.isEmpty)
    assert(stepzr2.next.isEmpty)
    assert(stepzar0.next.isEmpty)
    assert(stepzar1.next.nonEmpty)
  }

}
