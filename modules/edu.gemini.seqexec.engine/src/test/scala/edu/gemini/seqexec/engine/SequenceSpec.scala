package edu.gemini.seqexec.engine

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

  // TODO: Share these fixtures with StepSpec
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

  val seqz0: SequenceZ = SequenceZ("id", Nil, stepz0, Nil)
  val seqza0: SequenceZ = SequenceZ("id", Nil, stepza0, Nil)
  val seqza1: SequenceZ = SequenceZ("id", Nil, stepza1, Nil)
  val seqzr0: SequenceZ = SequenceZ("id", Nil, stepzr0, Nil)
  val seqzr1: SequenceZ = SequenceZ("id", Nil, stepzr1, Nil)
  val seqzr2: SequenceZ = SequenceZ("id", Nil, stepzr2, Nil)
  val seqzar0: SequenceZ = SequenceZ("id", Nil, stepzar0, Nil)
  val seqzar1: SequenceZ = SequenceZ("id", Nil, stepzar1, Nil)

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
