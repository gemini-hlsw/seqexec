// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import java.util.UUID
import seqexec.engine.Sequence.State.Final
import seqexec.model.{StepState, SequenceState}
import seqexec.model.enum.Instrument.GmosS
import seqexec.model.enum.Resource.TCS
import seqexec.model.{ActionType, UserDetails}
import fs2.async
import fs2.async.mutable.Semaphore
import fs2.Stream
import gem.Observation
import cats.implicits._
import cats.effect._
import monocle.Lens
import org.scalatest.Inside.inside
import org.scalatest.{FlatSpec, NonImplicitAssertions}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class packageSpec extends FlatSpec with NonImplicitAssertions {

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action  = fromIO(ActionType.Configure(TCS),
    for {
      _ <- IO(Thread.sleep(200))
    } yield Result.OK(Result.Configured(TCS)))

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = fromIO(ActionType.Configure(GmosS),
    for {
    _ <- IO(Thread.sleep(200))
  } yield Result.OK(Result.Configured(GmosS)))

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = fromIO(ActionType.Observe,
  for {
    _ <- IO(Thread.sleep(200))
  } yield Result.OK(Result.Observed("DummyFileId")))

  val faulty: Action  = fromIO(ActionType.Undefined,
  for {
    _ <- IO(Thread.sleep(100))
  } yield Result.Error("There was an error in this action"))

  private def always[D]: D => Boolean = _ => true

  val executions: List[List[Action]] = List(List(configureTcs, configureInst), List(observe))
  val seqId: Observation.Id = Observation.Id.unsafeFromString("GS-2019A-Q-0-1")
  val qs1: Engine.State =
    Engine.State(
      sequences = Map(
        (seqId,
          Sequence.State.init(
            Sequence(
              id = Observation.Id.unsafeFromString("GS-2018B-Q-0-2"),
              steps = List(
                Step.init(
                  id = 1,
                  fileId = None,
                  executions = List(
                    List(configureTcs, configureInst), // Execution
                    List(observe) // Execution
                  )
                ),
                Step.init(
                  id = 2,
                  fileId = None,
                  executions = executions
                )
              )
            )
          )
        )
      )
    )

  private val executionEngine = new Engine[Engine.State, Unit](Lens.id)
  private val user = UserDetails("telops", "Telops")

  def isFinished(status: SequenceState): Boolean = status match {
    case SequenceState.Idle      => true
    case SequenceState.Completed => true
    case SequenceState.Failed(_) => true
    case _                       => false
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def runToCompletion(s0: Engine.State): Option[Engine.State] = {
    executionEngine.process(Stream.eval(IO.pure(Event.start[executionEngine.ConcreteTypes](seqId, user, UUID.randomUUID(), always))))(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunSync.map(_._2)
  }

  it should "be in Running status after starting" in {
    val p = Stream.eval(IO.pure(Event.start[executionEngine.ConcreteTypes](seqId, user, UUID.randomUUID(), always)))
    val qs = executionEngine.process(p)(qs1).take(1).compile.last.unsafeRunSync.map(_._2)
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

  private def actionPause: Option[Engine.State] = {
    val s0: Engine.State = Engine.State(
      Map(seqId -> Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-1"),
        steps = List(
          Step.init(
            id = 1,
            fileId = None,
            executions = List(
              List(fromIO(ActionType.Undefined,
                IO(Result.Paused(new Result.PauseContext {}))))
            )
          )
        )
      ) ) )
    )
    val p = Stream.eval(IO.pure(Event.start[executionEngine.ConcreteTypes](seqId, user, UUID.randomUUID(), always)))

    //take(3): Start, Executing, Paused
    executionEngine.process(p)(s0).take(3).compile.last.unsafeRunSync.map(_._2)
  }

  "sequence state" should "stay as running when action pauses itself" in {
    assert(actionPause.exists(s => Sequence.State.isRunning(s.sequences(seqId))))
  }

  "engine" should "change action state to Paused if output is Paused" in {
    val r = actionPause
    assert(r.exists(_.sequences(seqId).current.execution.forall(Action.paused)))
  }

  "engine" should "run sequence to completion after resuming a paused action" in {
    val p = Stream.eval(IO.pure(Event.actionResume(seqId, 0, IO(Result.OK(Result.Configured(GmosS))))))

    val result = actionPause.flatMap(executionEngine.process(p)(_).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunTimed(5.seconds))
    val qso = result.flatMap(_.map(_._2))

    assert(qso.forall(x => x.sequences(seqId).current.actions.isEmpty && (x.sequences(seqId).status === SequenceState.Completed)))

  }

  "engine" should "keep processing input messages regardless of how long Actions take" in {
    val result = (for {
      q           <- Stream.eval(async.boundedQueue[IO, executionEngine.EventType](1))
      startedFlag <- Stream.eval(Semaphore.apply[IO](0))
      finishFlag  <- Stream.eval(Semaphore.apply[IO](0))
      r           <- {
        val qs = Engine.State(
          Map(seqId -> Sequence.State.init(Sequence(
            id = Observation.Id.unsafeFromString("GS-2018B-Q-0-2"),
            steps = List(
              Step.init(
                id = 1,
                fileId = None,
                executions = List(
                  List(fromIO(ActionType.Configure(TCS),
                    startedFlag.increment *> finishFlag.decrement *> IO.pure(Result.OK(Result.Configured(TCS)))
                  ))
                )
              )
            )
          )))
        )
        Stream.eval(List(
          List[IO[Unit]](
            q.enqueue1(Event.start[executionEngine.ConcreteTypes](seqId, user, UUID.randomUUID(), always)),
            startedFlag.decrement,
            q.enqueue1(Event.nullEvent),
            q.enqueue1(Event.getState[executionEngine.ConcreteTypes] { _ => Stream.eval(finishFlag.increment).map(_ => Event.nullEvent).some })
          ).sequence,
          executionEngine.process(q.dequeue)(qs).drop(1).takeThrough(a => !isFinished(a._2.sequences(seqId).status)).compile.drain
        ).parSequence)
      }
    } yield r ).compile.last.unsafeRunTimed(5.seconds).flatten

    assert(result.isDefined)
  }

  "engine" should "not capture runtime exceptions." in {
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def s0(e: Throwable): Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-4"),
        steps = List(
          Step.init(
            id = 1,
            fileId = None,
            executions = List(
              List(fromIO(ActionType.Undefined,
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
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-6"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 2, fileId = None, executions = executions),
          Step.init(id = 3, fileId = None, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped, StepState.Completed, StepState.Completed))
    }
  }

  it should "skip steps marked to be skipped in the middle of the sequence." in {
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-7"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions),
          Step.init(id = 2, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, fileId = None, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip several steps marked to be skipped." in {
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-8"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions),
          Step.init(id = 2, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 4, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 5, fileId = None, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped, StepState.Skipped, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip steps marked to be skipped at the end of the sequence." in {
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-9"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions),
          Step.init(id = 2, fileId = None, executions = executions),
          Step.init(id = 3, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true))
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId))) {
      case Some(s @ Final(_, SequenceState.Completed)) =>
        assert(s.done.map(Step.status) === List(StepState.Completed, StepState.Completed, StepState.Skipped))
    }
  }

  it should "skip a step marked to be skipped even if it is the only one." in {
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-10"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true))
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped))
    }
  }

  it should "skip steps marked to be skipped at the beginning of the sequence, even if they have breakpoints." in {
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-11"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 2, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 3, fileId = None, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip the leading steps if marked to be skipped, even if they have breakpoints and are the last ones." in {
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        Observation.Id.unsafeFromString("GS-2019A-Q-3"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions).copy(skipped = Step.Skipped(true)),
          Step.init(id = 2, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 2, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true))
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId))) {
      case Some(s@Final(_, SequenceState.Completed)) =>
        assert(s.done.map(Step.status) === List(StepState.Skipped, StepState.Skipped, StepState.Skipped))
    }
  }

  it should "skip steps marked to be skipped in the middle of the sequence, but honoring breakpoints." in {
    val s0: Engine.State = Engine.State(
      Map((seqId, Sequence.State.init(Sequence(
        id = Observation.Id.unsafeFromString("GS-2018B-Q-0-12"),
        steps = List(
          Step.init(id = 1, fileId = None, executions = executions),
          Step.init(id = 2, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, fileId = None, executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 4, fileId = None, executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped))
    }
  }

}
