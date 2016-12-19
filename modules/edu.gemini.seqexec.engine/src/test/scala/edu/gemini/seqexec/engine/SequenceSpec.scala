package edu.gemini.seqexec.engine

import edu.gemini.seqexec.model.Model.{SequenceMetadata, StepConfig}

import scalaz._
import Scalaz._
import org.scalatest.FlatSpec

import scalaz.concurrent.Task

/**
  * Created by jluhrs on 9/29/16.
  */
class SequenceSpec extends FlatSpec {

  // All tests check the output of running a sequence against the expected sequence of updates.

  // Use a small sequence and the list of expected updates.
  ignore should "run and generate the predicted sequence of updates." in {

  }

  // The hard part will be to make sure the sequence stops in a known point.
  ignore should "stop execution in response to a pause command" in {

  }

  // It should reuse code from the previous test to set the initial state for the test.
  ignore should "resume execution from the non-running state in response to a resume command." in {

  }

  ignore should "ignore pause command if sequence is not running." in {

  }

  // Be careful that resume command really arrives while sequence is running.
  ignore should "ignore resume command if sequence is already running." in {

  }

  //For this test, an action in one of the steps in the sequence must return an error.
  ignore should "stop execution and propagate error when an Action ends in error." in {

  }

  val metadata = SequenceMetadata("F2")

  // TODO: Share these fixtures with StepSpec
  val result = Result.OK(Unit)
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

  val seqz0: Sequence.Zipper   = Sequence.Zipper("id", metadata, Nil, stepz0, Nil)
  val seqza0: Sequence.Zipper  = Sequence.Zipper("id", metadata, Nil, stepza0, Nil)
  val seqza1: Sequence.Zipper  = Sequence.Zipper("id", metadata, Nil, stepza1, Nil)
  val seqzr0: Sequence.Zipper  = Sequence.Zipper("id", metadata, Nil, stepzr0, Nil)
  val seqzr1: Sequence.Zipper  = Sequence.Zipper("id", metadata, Nil, stepzr1, Nil)
  val seqzr2: Sequence.Zipper  = Sequence.Zipper("id", metadata, Nil, stepzr2, Nil)
  val seqzar0: Sequence.Zipper = Sequence.Zipper("id", metadata, Nil, stepzar0, Nil)
  val seqzar1: Sequence.Zipper = Sequence.Zipper("id", metadata, Nil, stepzar1, Nil)

  "next" should "be None when there are no more pending executions" in {
    assert(seqz0.next.isEmpty)
    assert(seqza0.next.isEmpty)
    assert(seqza1.next.nonEmpty)
    assert(seqzr0.next.isEmpty)
    assert(seqzr1.next.isEmpty)
    assert(seqzr2.next.isEmpty)
    assert(seqzar0.next.isEmpty)
    assert(seqzar1.next.nonEmpty)
  }

}
