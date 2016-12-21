package edu.gemini.seqexec.engine

import edu.gemini.seqexec.model.Model.StepConfig

import scalaz.syntax.either._
import org.scalatest.FlatSpec

import scalaz.concurrent.Task

import edu.gemini.seqexec.model.Model.StepState

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
  val failure = Result.Error(Unit)
  val action: Action = Task(result)
  val config: StepConfig = Map()
  val stepz0: Step.Zipper   = Step.Zipper(0, config, Nil, Execution.empty, Nil)
  val stepza0: Step.Zipper  = Step.Zipper(1, config, List(List(action)), Execution.empty, Nil)
  val stepza1: Step.Zipper  = Step.Zipper(1, config, List(List(action)), Execution(List(result.right)), Nil)
  val stepzr0: Step.Zipper  = Step.Zipper(1, config, Nil, Execution.empty, List(List(result)))
  val stepzr1: Step.Zipper  = Step.Zipper(1, config, Nil, Execution(List(result.right, result.right)), Nil)
  val stepzr2: Step.Zipper  = Step.Zipper(1, config, Nil, Execution(List(result.right, result.right)), List(List(result)))
  val stepzar0: Step.Zipper = Step.Zipper(1, config, Nil, Execution(List(result.right, action.left)), Nil)
  val stepzar1: Step.Zipper = Step.Zipper(1, config, List(List(action)), Execution(List(result.right, result.right)), List(List(result)))

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

  val step0: Step[Action] = Step(1, config, List(Nil))
  val step1: Step[Action] = Step(1, config, List(List(action)))
  val step2: Step[Action] = Step(2, config, List(List(action, action), List(action)))

  "currentify" should "be None only when a Step is empty of executions" in {
    assert(Step.Zipper.currentify(Step(0, config, Nil)).isEmpty)
    assert(Step.Zipper.currentify(step0).isEmpty)
    assert(Step.Zipper.currentify(step1).nonEmpty)
    assert(Step.Zipper.currentify(step2).nonEmpty)
  }

  "status" should "be Error when empty" in {
    assert(Step.status(stepz0.toStep) === StepState.Error("An action errored"))
  }

  "status" should "be Error when at least one Action failed" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          Map.empty,
          Nil,
          Execution(List(action.left, failure.right, result.right)),
          Nil
        ).toStep
      ) === StepState.Error("An action errored")
    )
  }

  "status" should "be Completed when all actions succeeded" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          Map.empty,
          Nil,
          Execution(List(result.right, result.right, result.right)),
          Nil
        ).toStep
      ) === StepState.Completed
    )
  }

  "status" should "be Running when there are both actions and results" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          Map.empty,
          Nil,
          Execution(List(result.right, action.left, result.right)),
          Nil
        ).toStep
      ) === StepState.Running
    )
  }

  "status" should "be Pending when there are only pending actions" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          Map.empty,
          Nil,
          Execution(List(action.left, action.left, action.left)),
          Nil
        ).toStep
      ) === StepState.Pending
    )
  }

}
