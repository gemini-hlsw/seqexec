// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.effect.concurrent.Semaphore
import cats.data.NonEmptyList
import cats.syntax.all._
import cats.effect._
import fs2.Stream
import seqexec.model.Observation
import org.scalatest.Inside.inside
import org.scalatest.NonImplicitAssertions
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger
import io.chrisdavenport.log4cats.Logger
import java.util.UUID
import java.util.concurrent.atomic.AtomicInteger
import seqexec.engine.Sequence.State.Final
import seqexec.model.{ClientId, SequenceState, StepState}
import seqexec.model.enum.Instrument.GmosS
import seqexec.model.enum.Resource.TCS
import seqexec.model.{ActionType, UserDetails}
import seqexec.engine.TestUtil.TestState
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext
import org.scalatest.flatspec.AnyFlatSpec

class packageSpec extends AnyFlatSpec with NonImplicitAssertions {

  implicit val ioContextShift: ContextShift[IO] =
    IO.contextShift(ExecutionContext.global)

  private implicit def logger: Logger[IO] = Slf4jLogger.getLoggerFromName[IO]("seqexec-engine")

  object DummyResult extends Result.RetVal

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action[IO] = fromF[IO](ActionType.Configure(TCS),
    for {
      _ <- IO(Thread.sleep(200))
    } yield Result.OK(DummyResult))

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action[IO] = fromF[IO](ActionType.Configure(GmosS),
    for {
    _ <- IO(Thread.sleep(200))
  } yield Result.OK(DummyResult))

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action[IO] = fromF[IO](ActionType.Observe,
  for {
    _ <- IO(Thread.sleep(200))
  } yield Result.OK(DummyResult))

  val faulty: Action[IO] = fromF[IO](ActionType.Undefined,
  for {
    _ <- IO(Thread.sleep(100))
  } yield Result.Error("There was an error in this action"))

  private val clientId: ClientId = ClientId(UUID.randomUUID)

  val executions: List[ParallelActions[IO]] =
    List(
      NonEmptyList.of(configureTcs, configureInst),
      NonEmptyList.one(observe))

  val seqId: Observation.Id = Observation.Id.unsafeFromString("GS-2019A-Q-0-1")
  val qs1: TestState =
    TestState(
      sequences = Map(
        (seqId,
          Sequence.State.init(
            Sequence(
              id = Observation.Id.unsafeFromString("GS-2018B-Q-0-2"),
              steps = List(
                Step.init(
                  id = 1,
                  executions = List(
                    NonEmptyList.of(configureTcs, configureInst), // Execution
                    NonEmptyList.one(observe) // Execution
                  )
                ),
                Step.init(
                  id = 2,
                  executions = executions
                )
              )
            )
          )
        )
      )
    )

  private val executionEngine = new Engine[IO, TestState, Unit](TestState)
  private val user = UserDetails("telops", "Telops")

  def isFinished(status: SequenceState): Boolean = status match {
    case SequenceState.Idle      => true
    case SequenceState.Completed => true
    case SequenceState.Failed(_) => true
    case _                       => false
  }

  def runToCompletion(s0: TestState): Option[TestState] = {
    executionEngine.process(PartialFunction.empty)(
      Stream.eval(
        IO.pure(Event.start[IO, TestState, Unit](seqId, user, clientId))
      )
    )(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunSync().map(_._2)
  }

  it should "be in Running status after starting" in {
    val p = Stream.eval(IO.pure(Event.start[IO, TestState, Unit](seqId, user, clientId)))
    val qs = executionEngine.process(PartialFunction.empty)(p)(qs1).take(1).compile.last.unsafeRunSync().map(_._2)
    assert(qs.exists(s => Sequence.State.isRunning(s.sequences(seqId))))
  }

  it should "be 0 pending executions after execution" in {
    val qs = runToCompletion(qs1)
    assert(qs.exists(_.sequences(seqId).pending.isEmpty))
  }

  it should "be 2 Steps done after execution" in {
    val qs = runToCompletion(qs1)
    assert(qs.exists(_.sequences(seqId).done.length === 2))
  }

  private def actionPause: Option[TestState] = {
    val s0: TestState = TestState(
      Map(seqId -> Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-1"),
        steps = List(
          Step.init(
            id = 1,
            executions = List(
              NonEmptyList.one(fromF[IO](ActionType.Undefined,
                IO(Result.Paused(new Result.PauseContext[IO] {}))))
            )
          )
        )
      ) ) )
    )
    val p = Stream.eval(IO.pure(Event.start[IO, TestState, Unit](seqId, user, clientId)))

    //take(3): Start, Executing, Paused
    executionEngine.process(PartialFunction.empty)(p)(s0).take(3).compile.last.unsafeRunSync().map(_._2)
  }

  "sequence state" should "stay as running when action pauses itself" in {
    assert(actionPause.exists(s => Sequence.State.isRunning(s.sequences(seqId))))
  }

  "engine" should "change action state to Paused if output is Paused" in {
    val r = actionPause
    assert(r.exists(_.sequences(seqId).current.execution.forall(Action.paused)))
  }

  "engine" should "run sequence to completion after resuming a paused action" in {
    val p = Stream.eval(IO.pure(Event.actionResume(seqId, 0, Stream.eval(IO(Result.OK(DummyResult))))))

    val result = actionPause.flatMap(executionEngine.process(PartialFunction.empty)(p)(_).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunTimed(5.seconds))
    val qso = result.flatMap(_.map(_._2))

    assert(qso.forall(x => x.sequences(seqId).current.actions.isEmpty && (x.sequences(seqId).status === SequenceState.Completed)))

  }

  "engine" should "keep processing input messages regardless of how long ParallelActions take" in {
    val result = (for {
      q           <- Stream.eval(fs2.concurrent.Queue.bounded[IO, executionEngine.EventType](1))
      startedFlag <- Stream.eval(Semaphore.apply[IO](0))
      finishFlag  <- Stream.eval(Semaphore.apply[IO](0))
      r           <- {
        val qs = TestState(
          Map(seqId -> Sequence.State.init(Sequence(
            id = Observation.Id.unsafeFromString("GS-2018B-Q-0-2"),
            steps = List(
              Step.init(
                id = 1,
                executions = List(
                  NonEmptyList.one(fromF[IO](ActionType.Configure(TCS),
                    startedFlag.release *> finishFlag.acquire *> IO.pure(Result.OK(DummyResult))
                  ))
                )
              )
            )
          )))
        )
        Stream.eval(List(
          List[IO[Unit]](
            q.enqueue1(Event.start[IO, TestState, Unit](seqId, user, clientId)),
            startedFlag.acquire,
            q.enqueue1(Event.nullEvent),
            q.enqueue1(Event.getState[IO, TestState, Unit] { _ => Stream.eval(finishFlag.release).as(Event.nullEvent[IO]).some })
          ).sequence,
          executionEngine.process(PartialFunction.empty)(q.dequeue)(qs).drop(1).takeThrough(a => !isFinished(a._2.sequences(seqId).status)).compile.drain
        ).parSequence)
      }
    } yield r ).compile.last.unsafeRunTimed(5.seconds).flatten

    assert(result.isDefined)
  }

  "engine" should "not capture runtime exceptions." in {
    def s0(e: Throwable): TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-4"),
        steps = List(
          Step.init(
            id = 1,
            executions = List(
              NonEmptyList.one(fromF[IO](ActionType.Undefined,
                IO.apply {
                  throw e
                }
              ) )
            )
          )
        )
      ) ) ) )
    )

    assertThrows[java.lang.RuntimeException](
      runToCompletion(s0(new java.lang.RuntimeException))
    )
  }

  it should "skip steps marked to be skipped at the beginning of the sequence." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-6"),
        steps = List(
          Step.init(id = 1, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 2, executions = executions),
          Step.init(id = 3, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(_.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped, StepState.Completed, StepState.Completed))
    }
  }

  it should "skip steps marked to be skipped in the middle of the sequence." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-7"),
        steps = List(
          Step.init(id = 1, executions = executions),
          Step.init(id = 2, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(_.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip several steps marked to be skipped." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-8"),
        steps = List(
          Step.init(id = 1, executions = executions),
          Step.init(id = 2, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 4, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 5, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(_.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped, StepState.Skipped, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip steps marked to be skipped at the end of the sequence." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-9"),
        steps = List(
          Step.init(id = 1, executions = executions),
          Step.init(id = 2, executions = executions),
          Step.init(id = 3, executions = executions).copy(skipMark = Step.SkipMark(true))
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId))) {
      case Some(s @ Final(_, SequenceState.Completed)) =>
        assert(s.done.map(_.status) === List(StepState.Completed, StepState.Completed, StepState.Skipped))
    }
  }

  it should "skip a step marked to be skipped even if it is the only one." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-10"),
        steps = List(
          Step.init(id = 1, executions = executions).copy(skipMark = Step.SkipMark(true))
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(_.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped))
    }
  }

  it should "skip steps marked to be skipped at the beginning of the sequence, even if they have breakpoints." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-11"),
        steps = List(
          Step.init(id = 1, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 2, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 3, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(_.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip the leading steps if marked to be skipped, even if they have breakpoints and are the last ones." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        Observation.Id.unsafeFromString("GS-2019A-Q-3"),
        steps = List(
          Step.init(id = 1, executions = executions).copy(skipped = Step.Skipped(true)),
          Step.init(id = 2, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 2, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true))
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId))) {
      case Some(s@Final(_, SequenceState.Completed)) =>
        assert(s.done.map(_.status) === List(StepState.Skipped, StepState.Skipped, StepState.Skipped))
    }
  }

  it should "skip steps marked to be skipped in the middle of the sequence, but honoring breakpoints." in {
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-12"),
        steps = List(
          Step.init(id = 1, executions = executions),
          Step.init(id = 2, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 4,  executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(_.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped))
    }
  }

  it should "run single Action" in {
    val dummy = new AtomicInteger(0)
    val markVal = 1
    val stepId = 1
    val s0: TestState = TestState(
      Map((seqId, Sequence.State.init(Sequence(
        id = seqId,
        steps = List(
          Step.init(id = stepId, executions = List(
            NonEmptyList.one(fromF[IO](ActionType.Undefined, IO{
              dummy.set(markVal)
              Result.OK(DummyResult)
            }))
          ))
        )
      ) ) ) )
    )

    val c = ActionCoordsInSeq(stepId, ExecutionIndex(0), ActionIndex(0))
    val event = Event.modifyState[IO, TestState, Unit](
      executionEngine.startSingle(ActionCoords(seqId, c)).void
    )
    val sfs = executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(event)))(s0)
      .map(_._2).take(2).compile.toList.unsafeRunSync()

    /**
     * First state update must have the action started.
     * Second state update must have the action finished.
     * The value in `dummy` must change. That is prove that the `Action` run.
     */
    inside (sfs) {
      case a::b::_ => {
        assert(TestState.sequenceStateIndex(seqId).getOption(a).exists(_.getSingleState(c).started))
        assert(TestState.sequenceStateIndex(seqId).getOption(b).exists(_.getSingleState(c)
          .completed))
        assert(dummy.get === markVal)
      }
    }
  }

  val qs2: TestState =
    TestState(
      sequences = Map(
        (seqId,
          Sequence.State.init(
            Sequence(
              id = Observation.Id.unsafeFromString("GS-2018B-Q-0-2"),
              steps = List(
                Step.init(
                  id = 1,
                  executions = List(
                    NonEmptyList.one(
                      Action[IO](ActionType.Undefined, Stream(Result.OK(DummyResult)).covary[IO],
                        Action.State(Action.ActionState.Completed(DummyResult), List.empty)
                      )
                    )
                  )
                ),
                Step.init(
                  id = 2,
                  executions = executions
                ),
                Step.init(
                  id = 3,
                  executions = executions
                ),
                Step.init(
                  id = 4,
                  executions = executions
                )
              )
            )
          )
        )
      )
    )

  it should "be able to start sequence from arbitrary step" in {
    val event = Event.modifyState[IO, TestState, Unit](
      executionEngine.startFrom(seqId, 3).void
    )

    val sf = executionEngine.process(PartialFunction.empty)(Stream.eval(IO.pure(event)))(qs2).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunSync().map(_._2)

    inside (sf.flatMap(_.sequences.get(seqId).map(_.toSequence))) {
      case Some(seq) => assertResult(Some(StepState.Completed))(seq.steps.get(0).map(_.status))
        assertResult(Some(StepState.Skipped))(seq.steps.get(1).map(_.status))
        assertResult(Some(StepState.Completed))(seq.steps.get(2).map(_.status))
        assertResult(Some(StepState.Completed))(seq.steps.get(3).map(_.status))
    }

  }

}
