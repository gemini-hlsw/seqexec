package edu.gemini.seqexec.engine

import edu.gemini.seqexec.engine.Event.start
import edu.gemini.seqexec.model.Model.SequenceState.{Error, Idle}
import edu.gemini.seqexec.model.Model.{Conditions, SequenceMetadata, SequenceState, StepConfig}

import scala.Function.const
import scalaz._
import Scalaz._
import org.scalatest.FlatSpec
import org.scalatest.Inside.inside
import org.scalatest.Matchers._

import scalaz.concurrent.Task
import scalaz.stream.async

/**
  * Created by jluhrs on 9/29/16.
  */
class SequenceSpec extends FlatSpec {

  val seqId ="TEST-01"

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

  val metadata = SequenceMetadata("F2", None)

  def simpleStep(id: Int, breakpoint: Boolean): Step[Action] =
    Step(
      id,
      None,
      config,
      Set.empty,
      breakpoint,
      List(
        List(action, action), // Execution
        List(action) // Execution
      )
    )

  def runToCompletion(q: scalaz.stream.async.mutable.Queue[Event], s0: Engine.State): Engine.State = {
    def isFinished(status: SequenceState): Boolean =
      status == Idle || status == SequenceState.Completed || status === Error

    q.enqueueOne(start(seqId)).flatMap(_ =>
      processE(q).drop(1).takeThrough(
        a => !isFinished(a._2.sequences(seqId).status)
      ).runLast.eval(s0)).unsafePerformSync.get._2
  }

  it should "stop on breakpoints" in {

    val q = async.boundedQueue[Event](10)
    val qs0: Engine.State =
      Engine.State(
        Conditions.default,
        None,
        Map(
          (seqId,
           Sequence.State.init(
             Sequence(
               seqId,
               SequenceMetadata("F2", None),
               List(simpleStep(1, breakpoint = false), simpleStep(2, breakpoint = true))
             )
           )
          )
        )
      )

    val qs1 = runToCompletion(q, qs0)

    inside (qs1.sequences(seqId)) {
      case Sequence.State.Zipper(zipper, status) =>
        status should be (Idle)
        assert(zipper.done.length == 1 && zipper.pending.isEmpty)
    }

  }

  it should "resume execution to completion after a breakpoint" in {

    val q = async.boundedQueue[Event](10)
    val qs0: Engine.State =
      Engine.State(
        Conditions.default,
        None,
        Map(
          (seqId,
           Sequence.State.init(
             Sequence(
               seqId,
               SequenceMetadata("F2", None),
               List(simpleStep(1, breakpoint = false), simpleStep(2, breakpoint = true), simpleStep(3, breakpoint = false))
             )
           )
          )
        )
      )

    val qs1 = runToCompletion(q, qs0)

    // Check that there is something left to run
    inside (qs1.sequences(seqId)) {
      case Sequence.State.Zipper(zipper, _) =>
        assert(zipper.pending.nonEmpty)
    }

    val qs2 = runToCompletion(q, qs1)

    inside (qs2.sequences(seqId)) {
      case f@Sequence.State.Final(_, status) =>
        status should be (SequenceState.Completed)
        assert(f.done.length == 3)
    }

  }

  // TODO: Share these fixtures with StepSpec
  val result = Result.OK(Result.Observed("dummyId"))
  val action: Action = Task(result)
  val config: StepConfig = Map()
  def simpleStep(pending: List[Actions], focus: Execution, done: List[Results]): Step.Zipper = {
    val rollback: (Execution, List[Actions]) =  done.map(_.map(const(action))) ++ List(focus.execution.map(const(action))) ++ pending match {
      case Nil => (Execution.empty, Nil)
      case x::xs => (Execution(x.map(_.left)), xs)
    }

    Step.Zipper(1, None, config, Set.empty, breakpoint = false, pending, focus, done, rollback)
  }
  val stepz0: Step.Zipper   = simpleStep(Nil, Execution.empty, Nil)
  val stepza0: Step.Zipper  = simpleStep(List(List(action)), Execution.empty, Nil)
  val stepza1: Step.Zipper  = simpleStep(List(List(action)), Execution(List(result.right)), Nil)
  val stepzr0: Step.Zipper  = simpleStep(Nil, Execution.empty, List(List(result)))
  val stepzr1: Step.Zipper  = simpleStep(Nil, Execution(List(result.right, result.right)), Nil)
  val stepzr2: Step.Zipper  = simpleStep(Nil, Execution(List(result.right, result.right)), List(List(result)))
  val stepzar0: Step.Zipper = simpleStep(Nil, Execution(List(result.right, action.left)), Nil)
  val stepzar1: Step.Zipper = simpleStep(List(List(action)), Execution(List(result.right, result.right)), List(List(result)))

  def simpleSequenceZipper(focus: Step.Zipper): Sequence.Zipper = Sequence.Zipper(seqId, metadata, Nil, focus, Nil)
  val seqz0: Sequence.Zipper   = simpleSequenceZipper(stepz0)
  val seqza0: Sequence.Zipper  = simpleSequenceZipper(stepza0)
  val seqza1: Sequence.Zipper  = simpleSequenceZipper(stepza1)
  val seqzr0: Sequence.Zipper  = simpleSequenceZipper(stepzr0)
  val seqzr1: Sequence.Zipper  = simpleSequenceZipper(stepzr1)
  val seqzr2: Sequence.Zipper  = simpleSequenceZipper(stepzr2)
  val seqzar0: Sequence.Zipper = simpleSequenceZipper(stepzar0)
  val seqzar1: Sequence.Zipper = simpleSequenceZipper(stepzar1)

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

