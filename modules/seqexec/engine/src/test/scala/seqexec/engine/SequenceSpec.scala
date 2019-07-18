// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.effect.{ContextShift, IO}
import fs2.Stream
import gem.Observation
import java.util.UUID
import org.scalatest.FlatSpec
import org.scalatest.Inside.inside
import org.scalatest.Matchers._
import seqexec.model.{ActionType, ClientId, SequenceState, UserDetails}
import scala.Function.const
import seqexec.engine.TestUtil.TestState

import scala.concurrent.ExecutionContext

class SequenceSpec extends FlatSpec {

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  private val seqId = Observation.Id.unsafeFromString("GS-2018A-Q-0-1")

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

  private val user = UserDetails("telops", "Telops")
  private val executionEngine = new Engine[TestState, Unit](TestState)

  private def always[D]: D => Boolean = _ => true

  def simpleStep(id: Int, breakpoint: Boolean): Step[IO] =
    Step.init(
      id = id,
      executions = List(
        List(action, action), // Execution
        List(action) // Execution
      )
    ).copy(breakpoint = Step.BreakpointMark(breakpoint))

  def isFinished(status: SequenceState): Boolean = status match {
    case SequenceState.Idle      => true
    case SequenceState.Completed => true
    case SequenceState.Failed(_) => true
    case _                       => false
  }

  def runToCompletion(s0: TestState): Option[TestState] = {
    executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(Event.start[executionEngine.ConcreteTypes](seqId, user, ClientId(UUID.randomUUID), always))))(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunSync.map(_._2)
  }

  it should "stop on breakpoints" in {

    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(simpleStep(1, breakpoint = false), simpleStep(2, breakpoint = true))
              )
            )
          )
        )
      )

    val qs1 = runToCompletion(qs0)

    inside (qs1.map(_.sequences(seqId))) {
      case Some(Sequence.State.Zipper(zipper, status, _)) =>
        assert(zipper.done.length == 1 && zipper.pending.isEmpty)
        status should be (SequenceState.Idle)
    }

  }

  it should "resume execution to completion after a breakpoint" in {

    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(simpleStep(1, breakpoint = false), simpleStep(2, breakpoint = true), simpleStep(3, breakpoint = false))
              )
            )
          )
        )
      )

    val qs1 = runToCompletion(qs0)

    // Check that there is something left to run
    inside (qs1.map(_.sequences(seqId))) {
      case Some(Sequence.State.Zipper(zipper, _, _)) =>
        assert(zipper.pending.nonEmpty)
    }

    val qs2 = qs1.flatMap(runToCompletion)

    inside (qs2.map(_.sequences(seqId))) {
      case Some(f@Sequence.State.Final(_, status)) =>
        assert(f.done.length == 3)
        status should be (SequenceState.Completed)
    }

  }

  // TODO: Share these fixtures with StepSpec
  private object DummyResult extends Result.RetVal
  private val result: Result[Nothing] = Result.OK(DummyResult)
  private val action: Action[IO] = fromF[IO](ActionType.Undefined, IO(result))
  private val completedAction: Action[IO] = action.copy(state = Action.State(Action.ActionState.Completed(DummyResult), Nil))
  def simpleStep2(pending: List[Actions[IO]], focus: Execution[IO], done: List[Results[IO]]): Step.Zipper[IO] = {
    val rollback: (Execution[IO], List[Actions[IO]]) =  done.map(_.map(const(action))) ++ List(focus.execution.map(const(action))) ++ pending match {
      case Nil => (Execution.empty, Nil)
      case x::xs => (Execution(x), xs)
    }

    Step.Zipper(1, breakpoint = Step.BreakpointMark(false), Step.SkipMark(false), pending, focus, done.map(_.map{r =>
      val x = fromF[IO](ActionType.Observe, IO(r))
      x.copy(state = Execution.actionStateFromResult(r)(x.state))
    }), rollback)
  }
  val stepz0: Step.Zipper[IO]   = simpleStep2(Nil, Execution.empty, Nil)
  val stepza0: Step.Zipper[IO]  = simpleStep2(List(List(action)), Execution.empty, Nil)
  val stepza1: Step.Zipper[IO]  = simpleStep2(List(List(action)), Execution(List(completedAction)), Nil)
  val stepzr0: Step.Zipper[IO]  = simpleStep2(Nil, Execution.empty, List(List(result)))
  val stepzr1: Step.Zipper[IO]  = simpleStep2(Nil, Execution(List(completedAction, completedAction)), Nil)
  val stepzr2: Step.Zipper[IO]  = simpleStep2(Nil, Execution(List(completedAction, completedAction)), List(List(result)))
  val stepzar0: Step.Zipper[IO] = simpleStep2(Nil, Execution(List(completedAction, action)), Nil)
  val stepzar1: Step.Zipper[IO] = simpleStep2(List(List(action)), Execution(List(completedAction, completedAction)), List(List(result)))

  def simpleSequenceZipper(focus: Step.Zipper[IO]): Sequence.Zipper[IO] = Sequence.Zipper(seqId, Nil, focus, Nil)
  val seqz0: Sequence.Zipper[IO]   = simpleSequenceZipper(stepz0)
  val seqza0: Sequence.Zipper[IO]  = simpleSequenceZipper(stepza0)
  val seqza1: Sequence.Zipper[IO]  = simpleSequenceZipper(stepza1)
  val seqzr0: Sequence.Zipper[IO]  = simpleSequenceZipper(stepzr0)
  val seqzr1: Sequence.Zipper[IO]  = simpleSequenceZipper(stepzr1)
  val seqzr2: Sequence.Zipper[IO]  = simpleSequenceZipper(stepzr2)
  val seqzar0: Sequence.Zipper[IO] = simpleSequenceZipper(stepzar0)
  val seqzar1: Sequence.Zipper[IO] = simpleSequenceZipper(stepzar1)

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

  "startSingle" should "mark a single Action as started" in {
    val seq = Sequence.State.init(
      Sequence(
          id = seqId,
          steps = List(simpleStep(1, breakpoint = false), simpleStep(2, breakpoint = false))
        )
      )

    val c = ActionCoordsInSeq(1, ExecutionIndex(0), ActionIndex(1))

    assert(seq.startSingle(c).getSingleState(c) === Action.ActionState.Started)

  }

  it should "not start single Action from completed Step" in {
    val seq1 = Sequence.State.init(
      Sequence(
        id = seqId,
        steps = List(
          Step.init(
            id = 1,
            executions = List(
              List(completedAction, completedAction), // Execution
              List(completedAction) // Execution
            )
          ),
          Step.init(
            id = 2,
            executions = List(
              List(action, action), // Execution
              List(action) // Execution
            )
          )
        )
      )
    )
    val seq2 = Sequence.State.Final(
      Sequence(
        id = seqId,
        steps = List(
          Step.init(
            id = 1,
            executions = List(
              List(completedAction, completedAction), // Execution
              List(completedAction) // Execution
            )
          )
        )
      ),
      SequenceState.Completed
    )
    val c1 = ActionCoordsInSeq(1, ExecutionIndex(0), ActionIndex(0))

    assert(seq1.startSingle(c1).getSingleState(c1).isIdle)
    assert(seq2.startSingle(c1).getSingleState(c1).isIdle)

  }

  "failSingle" should "mark a single running Action as failed" in {
    val c = ActionCoordsInSeq(1, ExecutionIndex(0), ActionIndex(0))
    val seq = Sequence.State.init(
      Sequence(
        id = seqId,
        steps = List(
          Step.init(
            id = 1,
            executions = List(
              List(action, action), // Execution
              List(action) // Execution
            )
          )
        )
      )
    ).startSingle(c)
    val c2 = ActionCoordsInSeq(1, ExecutionIndex(1), ActionIndex(0))

    assert(seq.failSingle(c, Result.Error("")).getSingleState(c).errored)
    assert(seq.failSingle(c2, Result.Error("")).getSingleState(c2).isIdle)
  }

  "completeSingle" should "mark a single running Action as completed" in {
    val c = ActionCoordsInSeq(1, ExecutionIndex(0), ActionIndex(0))
    val seq = Sequence.State.init(
      Sequence(
        id = seqId,
        steps = List(
          Step.init(
            id = 1,
            executions = List(
              List(action, action), // Execution
              List(action) // Execution
            )
          )
        )
      )
    ).startSingle(c)

    assert(seq.completeSingle(c, DummyResult).getSingleState(c).completed)
  }

}
