// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.effect.{ ContextShift, IO }
import cats.effect.Timer
import cats.effect.concurrent.Ref
import cats.data.NonEmptyList
import cats.tests.CatsSuite
import fs2.concurrent.Queue
import fs2.Stream
import gem.Observation
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import java.util.UUID
import org.scalatest.Inside._
import seqexec.engine.TestUtil.TestState
import seqexec.engine.EventResult._
import seqexec.engine.SystemEvent._
import seqexec.model.enum.Instrument.GmosS
import seqexec.model.{ActionType, ClientId, SequenceState, StepState, UserDetails}
import seqexec.model.enum.Resource
import seqexec.model.{ActionType, UserDetails}
import scala.Function.const
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class StepSpec extends CatsSuite {

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  implicit val ioTimer: Timer[IO] = IO.timer(ExecutionContext.global)

  private implicit def L: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("seqexec")

  private val seqId = Observation.Id.unsafeFromString("GS-2017B-Q-1-1")
  private val user = UserDetails("telops", "Telops")

  private val executionEngine = new Engine[IO, TestState, Unit](TestState)

  private object DummyResult extends Result.RetVal with Serializable
  private val result = Result.OK(DummyResult)
  private val failure = Result.Error("Dummy error")
  private val actionFailed =  fromF[IO](ActionType.Undefined, IO(failure)).copy(state = Action.State
  (Action.ActionState.Failed(failure), Nil))
  private val action: Action[IO] = fromF[IO](ActionType.Undefined, IO(result))
  private val actionCompleted: Action[IO] = action.copy(state = Action.State(Action.ActionState.Completed(DummyResult), Nil))
  private def always[D]: D => Boolean = _ => true
  private val clientId: ClientId = ClientId(UUID.randomUUID)

  def simpleStep(pending: List[ParallelActions[IO]], focus: Execution[IO], done: List[NonEmptyList[Result[IO]]]): Step.Zipper[IO] = {
    val rollback: (Execution[IO], List[ParallelActions[IO]]) = {
        val doneParallelActions: List[ParallelActions[IO]] = done.map(_.map(const(action)))
        val focusParallelActions: List[ParallelActions[IO]] = focus.toParallelActionsList
        doneParallelActions ++ focusParallelActions ++ pending match {
        case Nil => (Execution.empty, Nil)
        case x::xs => (Execution(x.toList), xs)
      }
    }

    Step.Zipper(
      id = 1,
      breakpoint = Step.BreakpointMark(false),
      skipMark = Step.SkipMark(false),
      pending = pending,
      focus = focus,
      done = done.map(_.map{ r =>
        val x = fromF[IO](ActionType.Observe, IO(r))
        x.copy(state = Execution.actionStateFromResult(r)(x.state))
      }),
      rolledback = rollback)
  }

  val stepz0: Step.Zipper[IO]   = simpleStep(Nil, Execution.empty, Nil)
  val stepza0: Step.Zipper[IO]  = simpleStep(List(NonEmptyList.one(action)), Execution.empty, Nil)
  val stepza1: Step.Zipper[IO]  = simpleStep(List(NonEmptyList.one(action)), Execution(List(actionCompleted))
    , Nil)
  val stepzr0: Step.Zipper[IO]  = simpleStep(Nil, Execution.empty, List(NonEmptyList.one(result)))
  val stepzr1: Step.Zipper[IO]  = simpleStep(Nil, Execution(List(actionCompleted,
    actionCompleted)), Nil)
  val stepzr2: Step.Zipper[IO]  = simpleStep(Nil, Execution(List(actionCompleted,
    actionCompleted)), List(NonEmptyList.one(result)))
  val stepzar0: Step.Zipper[IO] = simpleStep(Nil, Execution(List(actionCompleted, action)), Nil)
  val stepzar1: Step.Zipper[IO] = simpleStep(List(NonEmptyList.one(action)), Execution(List(actionCompleted,
    actionCompleted)), List(NonEmptyList.one(result)))
  private val startEvent = Event.start[IO, TestState, Unit](seqId, user, clientId, always)

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action[IO] = fromF[IO](ActionType.Configure(Resource.TCS),
    for {
      _ <- L.info("System: Start TCS configuration")
      _ <- IO.sleep(new FiniteDuration(200, MILLISECONDS))
      _ <- L.info("System: Complete TCS configuration")
    } yield Result.OK(DummyResult))

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action[IO] = fromF[IO](ActionType.Configure(GmosS),
    for {
      _ <- L.info("System: Start Instrument configuration")
      _ <- IO.sleep(new FiniteDuration(150, MILLISECONDS))
      _ <- L.info("System: Complete Instrument configuration")
    } yield Result.OK(DummyResult))

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action[IO] = fromF[IO](ActionType.Observe,
    for {
    _ <- L.info("System: Start observation")
    _ <- IO.sleep(new FiniteDuration(200, MILLISECONDS))
    _ <- L.info("System: Complete observation")
  } yield Result.OK(DummyResult))

  def error(errMsg: String): Action[IO] = fromF[IO](ActionType.Undefined,
    IO.sleep(new FiniteDuration(200, MILLISECONDS)) *>
      Result.Error(errMsg).pure[IO]
  )

  def aborted: Action[IO] = fromF[IO](ActionType.Undefined,
    IO.sleep(new FiniteDuration(200, MILLISECONDS)) *>
      Result.OKAborted(DummyResult).pure[IO]
  )

  def errorSet1(errMsg: String): (Ref[IO, Int], Action[IO]) = {
    val ref = Ref.unsafe[IO, Int](0)
    val action = fromF[IO](ActionType.Undefined,
      ref.update(_ + 1).as(Result.OK(DummyResult)),
      Result.Error(errMsg).pure[IO],
      ref.update(_ + 1).as(Result.OK(DummyResult)))
    (ref, action)
  }

  def errorSet2(errMsg: String): Action[IO] =
    fromF[IO](ActionType.Undefined,
      Result.Error(errMsg).pure[IO])

  def fatalError(errMsg: String): Action[IO] =
    fromF[IO](ActionType.Undefined,
      IO.raiseError(new RuntimeException(errMsg)))

  def triggerPause(q: Queue[IO, executionEngine.EventType]): Action[IO] = fromF[IO](ActionType.Undefined,
    for {
      _ <- q.enqueue1(Event.pause(seqId, user))
      // There is not a distinct result for Pause because the Pause action is a
      // trick for testing but we don't need to support it in real life, the pause
      // input event is enough.
    } yield Result.OK(DummyResult))

  def triggerStart(q: IO[Queue[IO, executionEngine.EventType]]): Action[IO] = fromF[IO](ActionType.Undefined,
    for {
      _ <- q.map(_.enqueue1(Event.start(seqId, user, clientId, always)))
      // Same case that the pause action
    } yield Result.OK(DummyResult))

  def isFinished(status: SequenceState): Boolean = status match {
    case SequenceState.Idle      => true
    case SequenceState.Completed => true
    case SequenceState.Failed(_) => true
    case SequenceState.Aborted   => true
    case _                       => false
  }

  def runToCompletion(s0: TestState): Option[TestState] = {
    executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(Event.start[IO, TestState, Unit](seqId, user, clientId, always))))(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunSync.map(_._2)
  }

  def runToCompletionL(s0: TestState): List[TestState] = {
    executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(Event.start[IO, TestState, Unit](seqId, user, clientId, always))))(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.toVector.unsafeRunSync.map(_._2).toList
  }

  // This test must have a simple step definition and the known sequence of updates that running that step creates.
  // The test will just run step and compare the output with the predefined sequence of updates.

  // The difficult part is to set the pause command to interrupts the step execution in the middle.
  test("pause should stop execution in response to a pause command") {
    val q: Stream[IO, Queue[IO, executionEngine.EventType]] = Stream.eval(Queue.bounded[IO, executionEngine.EventType](10))
    def qs0(q: Queue[IO, executionEngine.EventType]): TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.of(configureTcs, configureInst, triggerPause(q)), // Execution
                      NonEmptyList.one(observe) // Execution
                    )
                  )
                )
              )
            )
          )
        )
      )

    def notFinished(v: (executionEngine.ResultType, TestState)): Boolean =
      !isFinished(v._2.sequences(seqId).status)

    val m = for {
      k <- q
      o <- Stream.apply(qs0(k))
      _ <- Stream.eval(k.enqueue1(startEvent))
      u <- executionEngine.process(PartialFunction.empty)(k.dequeue)(o).drop(1).takeThrough(notFinished).map(_._2)
    } yield u.sequences(seqId)

    inside (m.compile.last.unsafeRunSync()) {
      case Some(Sequence.State.Zipper(zipper, status, _)) =>
        inside (zipper.focus.toStep) {
          case Step(_, _, _, _, List(ex1, ex2)) =>
            assert( Execution(ex1.toList).results.length == 3 && Execution(ex2.toList).actions.length == 1)
        }
        status should be (SequenceState.Idle)
    }

  }

  test("resume execution from the non-running state in response to a resume command, rolling back a partially run step.") {
    // Engine state with one idle sequence partially executed. One Step completed, two to go.
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.Zipper(
              Sequence.Zipper(
                id = Observation.Id.unsafeFromString("GS-2018A-Q-3-1"),
                pending = Nil,
                focus = Step.Zipper(
                  id = 2,
                  breakpoint = Step.BreakpointMark(false),
                  skipMark = Step.SkipMark(false),
                  pending = Nil,
                  focus = Execution(List(observe)),
                  done = List(NonEmptyList.of(actionCompleted, actionCompleted)),
                  rolledback = (Execution(List(configureTcs, configureInst)), List(NonEmptyList.one(observe)))),
                done = Nil
              ),
              SequenceState.Idle,
              Map.empty
            )
          )
        )
      )

    val qs1: Stream[IO, Option[Sequence.State[IO]]] =
      for {
        u <- executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(startEvent)))(qs0).take(1)
      } yield u._2.sequences.get(seqId)

    inside (qs1.compile.last.unsafeRunSync()) {
      case Some(Some(Sequence.State.Zipper(zipper, status, _))) =>
        inside (zipper.focus.toStep) {
          case Step(_, _, _, _, List(ex1, ex2)) =>
            assert(Execution(ex1.toList).actions.length == 2 && Execution(ex2.toList).actions.length == 1)
        }
        assert(status.isRunning)
    }

  }

  test("cancel a pause request in response to a cancel pause command.") {
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.Zipper(
              Sequence.Zipper(
                id = Observation.Id.unsafeFromString("GN-2017A-Q-7-1"),
                pending = Nil,
                focus = Step.Zipper(
                  id = 2,
                  breakpoint = Step.BreakpointMark(false),
                  skipMark = Step.SkipMark(false),
                  pending = Nil,
                  focus = Execution(List(observe)),
                  done = List(NonEmptyList.of(actionCompleted, actionCompleted)),
                  rolledback = (Execution(List(configureTcs, configureInst)), List(NonEmptyList.one(observe)))),
                done = Nil
              ),
              SequenceState.Running(userStop = true, internalStop = false),
              Map.empty
            )
          )
        )
      )

    val qs1 = executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(Event.cancelPause[IO, TestState, Unit](seqId, user))))(qs0).take(1).compile.last.unsafeRunSync.map(_._2)

    inside (qs1.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Zipper(_, status, _)) =>
        assert(status.isRunning)
    }

  }

  test("engine should test pause command if step is not being executed.") {
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.of(configureTcs, configureInst), // Execution
                      NonEmptyList.one(observe) // Execution
                    )
                  )
                )
              )
            )
          )
        )
      )
    val qss = executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(Event.pause[IO, TestState, Unit](seqId, user))))(qs0).take(1).compile.last.unsafeRunSync.map(_._2)

    inside (qss.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Zipper(zipper, status, _)) =>
        inside (zipper.focus.toStep) {
          case Step(_, _, _, _, List(ex1, ex2)) =>
            assert( Execution(ex1.toList).actions.length == 2 && Execution(ex2.toList).actions.length == 1)
        }
        status should be (SequenceState.Idle)
    }
  }

  // Be careful that start command doesn't run an already running sequence.
  test("engine test start command if step is already running.") {
    val q = Queue.bounded[IO, executionEngine.EventType](10)
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.of(configureTcs, configureInst), // Execution
                      NonEmptyList.one(triggerStart(q)), // Execution
                      NonEmptyList.one(observe) // Execution
                    )
                  )
                )
              )
            )
          )
        )
      )

    val qss = q.flatMap { k => k.enqueue1(Event.start(seqId, user, clientId, always)).flatMap(_ => executionEngine.process(PartialFunction.empty)(k.dequeue)(qs0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.toVector)}.unsafeRunSync

    val actionsCompleted = qss.map(_._1).collect{case SystemUpdate(x: Completed[_], _) => x}
    assert(actionsCompleted.length == 4)

    val executionsCompleted = qss.map(_._1).collect{case SystemUpdate(x: Executed, _) => x}
    assert(executionsCompleted.length == 3)

    val sequencesCompleted = qss.map(_._1).collect{case SystemUpdate(x: Finished, _) => x}
    assert(sequencesCompleted.length == 1)

    inside (qss.lastOption.flatMap(_._2.sequences.get(seqId))) {
      case Some(Sequence.State.Final(_, status)) =>
        status should be (SequenceState.Completed)
    }
  }

  // For this test, one of the actions in the step must produce an error as result.
  test("engine should stop execution and propagate error when an Action ends in error.") {
    val errMsg = "Dummy error"
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.of(configureTcs, configureInst), // Execution
                      NonEmptyList.one(error(errMsg)),
                      NonEmptyList.one(observe)
                    )
                  )
                )
              )
            )
          )
        )
      )

    val qs1 = runToCompletion(qs0)

    inside (qs1.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Zipper(zipper, status, _)) =>
        inside (zipper.focus.toStep) {
          // Check that the sequence stopped midway
          case Step(_, _, _, _, List(ex1, ex2, ex3)) =>
            assert( Execution(ex1.toList).results.length == 2 && Execution(ex2.toList).results.length == 1 && Execution(ex3.toList).actions.length == 1)
        }
        // And that it ended in error
        status should be (SequenceState.Failed(errMsg))
    }
  }

  test("engine should complete execution and propagate error when a partial Action ends in error.") {
    val errMsg = "Dummy error"
    val (ref, action) = errorSet1(errMsg)
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.one(action)
                    )
                  )
                )
              )
            )
          )
        )
      )

    val qs1 = runToCompletion(qs0)

    inside (qs1.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Final(_, status)) =>
        // Without the error we should have a value 2
        ref.get.unsafeRunSync() shouldBe(1)
        // And that it ended in error
        status shouldBe SequenceState.Completed
    }
  }

  test("engine should mark a step as aborted if the action ends as aborted") {
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.one(aborted)
                    )
                  )
                )
              )
            )
          )
        )
      )

    val qs1 = runToCompletion(qs0)

    inside (qs1.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Zipper(_, status, _)) =>
        // And that it ended in aborted
        status shouldBe SequenceState.Aborted
    }
  }

  test("engine should stop execution and propagate error when a single partial Action fails") {
    val errMsg = "Dummy error"
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.one(errorSet2(errMsg))
                    )
                  )
                )
              )
            )
          )
        )
      )

    val qs1 = runToCompletion(qs0)

    inside (qs1.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Zipper(_, status, _)) =>
        // Without the error we should have a value 2
        // And that it ended in error
        status should be (SequenceState.Failed(errMsg))
    }
  }

  test("engine should let fatal errors bubble") {
    val errMsg = "Dummy error"
    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.one(fatalError(errMsg))
                    )
                  )
                )
              )
            )
          )
        )
      )

    a [RuntimeException] should be thrownBy runToCompletion(qs0)

  }

  test("engine should record a partial result and continue execution.") {

    // For result types
    case class RetValDouble(v: Double) extends Result.RetVal
    case class PartialValDouble(v: Double) extends Result.PartialVal

    val qs0: TestState =
      TestState(
        sequences = Map(
          (seqId,
            Sequence.State.init(
              Sequence(
                id = seqId,
                steps = List(
                  Step.init(
                    id = 1,
                    executions = List(
                      NonEmptyList.one(
                        Action(
                          ActionType.Undefined,
                          Stream.emits(List(
                            Result.Partial(PartialValDouble(0.5)),
                            Result.OK(RetValDouble(1.0))
                          )).covary[IO],
                          Action.State(Action.ActionState.Idle, Nil)
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    val qss = runToCompletionL(qs0)

    inside (qss.drop(1).headOption.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Zipper(zipper, status, _)) =>
        inside (zipper.focus.focus.execution.headOption) {
          case Some(Action(_, _, Action.State(Action.ActionState.Started, v::_))) => v shouldEqual PartialValDouble(0.5)
        }
        assert(status.isRunning)
    }
    inside (qss.lastOption.flatMap(_.sequences.get(seqId))) {
      case Some(Sequence.State.Final(seq, status)) =>
        seq.steps.headOption.flatMap(_.executions.headOption.map(_.head)).map(_.state.runState) shouldEqual Some(Action.ActionState.Completed(RetValDouble(1.0)))
        status shouldBe SequenceState.Completed
    }

  }

  test("uncurrentify should be None when not all executions are completed") {
    assert(stepz0.uncurrentify.isEmpty)
    assert(stepza0.uncurrentify.isEmpty)
    assert(stepza1.uncurrentify.isEmpty)
    assert(stepzr0.uncurrentify.isEmpty)
    assert(stepzr1.uncurrentify.nonEmpty)
    assert(stepzr2.uncurrentify.nonEmpty)
    assert(stepzar0.uncurrentify.isEmpty)
    assert(stepzar1.uncurrentify.isEmpty)
  }

  test("next should be None when there are no more pending executions") {
    assert(stepz0.next.isEmpty)
    assert(stepza0.next.isEmpty)
    assert(stepza1.next.nonEmpty)
    assert(stepzr0.next.isEmpty)
    assert(stepzr1.next.isEmpty)
    assert(stepzr2.next.isEmpty)
    assert(stepzar0.next.isEmpty)
    assert(stepzar1.next.nonEmpty)
  }

  val step0: Step[IO] = Step.init(1, Nil)
  val step1: Step[IO] = Step.init(1, List(NonEmptyList.one(action)))
  val step2: Step[IO] = Step.init(2, List(NonEmptyList.of(action, action), NonEmptyList.one(action)))

  test("currentify should be None only when a Step is empty of executions") {
    assert(Step.Zipper.currentify(Step.init(0, Nil)).isEmpty)
    assert(Step.Zipper.currentify(step0).isEmpty)
    assert(Step.Zipper.currentify(step1).nonEmpty)
    assert(Step.Zipper.currentify(step2).nonEmpty)
  }

  test("status should be completed when it doesn't have any executions") {
    assert(stepz0.toStep.status === StepState.Completed)
  }

  test("status should be Error when at least one Action failed") {
    assert(
      Step.Zipper(
        id = 1,
        breakpoint = Step.BreakpointMark(false),
        skipMark = Step.SkipMark(false),
        pending = Nil,
        focus = Execution(List(action, actionFailed, actionCompleted)),
        done = Nil,
        rolledback = (Execution(List(action, action, action)), Nil)
      ).toStep.status === StepState.Failed("Dummy error")
    )
  }

  test("status should be Completed when all actions succeeded") {
    assert(
      Step.Zipper(
        id = 1,
        breakpoint = Step.BreakpointMark(false),
        skipMark = Step.SkipMark(false),
        pending = Nil,
        focus = Execution(List(actionCompleted, actionCompleted, actionCompleted)),
        done = Nil,
        rolledback = (Execution(List(action, action, action)), Nil)
      ).toStep.status === StepState.Completed
    )
  }

  test("status should be Running when there are both actions and results") {
    assert(
      Step.Zipper(
        id = 1,
        breakpoint = Step.BreakpointMark(false),
        skipMark = Step.SkipMark(false),
        pending = Nil,
        focus = Execution(List(actionCompleted, action, actionCompleted)),
        done = Nil,
        rolledback = (Execution(List(action, action, action)), Nil)
      ).toStep.status === StepState.Running
    )
  }

  test("status should be Pending when there are only pending actions") {
    assert(
      Step.Zipper(
        id = 1,
        breakpoint = Step.BreakpointMark(false),
        skipMark = Step.SkipMark(false),
        pending = Nil,
        focus = Execution(List(action, action, action)),
        done = Nil,
        rolledback = (Execution(List(action, action, action)), Nil)
      ).toStep.status === StepState.Pending
    )
  }

  test("status should be Skipped if the Step was skipped") {
    assert(
      Step.Zipper(
        id = 1,
        breakpoint = Step.BreakpointMark(false),
        skipMark = Step.SkipMark(false),
        pending = Nil,
        focus = Execution(List(action, action, action)),
        done = Nil,
        rolledback = (Execution(List(action, action, action)), Nil)
      ).toStep.copy(skipped = Step.Skipped(true)).status === StepState.Skipped
    )
  }

}
