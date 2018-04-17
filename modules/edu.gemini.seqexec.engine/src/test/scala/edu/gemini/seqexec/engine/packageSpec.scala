// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import java.util.UUID

import edu.gemini.seqexec.engine.Sequence.State.Final
import edu.gemini.seqexec.model.Model.StepState
import edu.gemini.seqexec.model.Model.Operator
import edu.gemini.seqexec.model.Model.{Resource, SequenceMetadata, SequenceState, StepConfig}
import edu.gemini.seqexec.model.Model.Instrument.{F2, GmosS}
import edu.gemini.seqexec.model.Model.Resource.TCS
import edu.gemini.seqexec.model.{ActionType, UserDetails}
import fs2.async
import fs2.async.mutable.Semaphore
import fs2.Stream

import cats.data.Kleisli
import cats.implicits._
import cats.effect._
import monocle.Lens
import monocle.macros.GenLens
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

  val executions: List[List[Action]] = List(List(configureTcs, configureInst), List(observe))
  val config: StepConfig = Map()
  val seqId: String = "TEST-01"
  val qs1: Engine.State[Unit] =
    Engine.State[Unit](
      userData = (),
      sequences = Map(
        (seqId,
          Sequence.State.init(
            Sequence(
              id = "First",
              metadata = SequenceMetadata(F2, None, ""),
              steps = List(
                Step.init(
                  id = 1,
                  fileId = None,
                  config = config,
                  resources = Set(Resource.TCS, F2),
                  executions = List(
                    List(configureTcs, configureInst), // Execution
                    List(observe) // Execution
                  )
                ),
                Step.init(
                  id = 2,
                  fileId = None,
                  config = config,
                  resources = Set(Resource.TCS, Resource.OI, F2),
                  executions = executions
                )
              )
            )
          )
        )
      )
    )

  private val seqG =
    Sequence.State.init(
      Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(
            1,
            fileId = None,
            config = config,
            resources = Set(GmosS),
            executions = executions
          )
        )
      )
    )

  implicit object UnitCanGenerateActionMetadata extends ActionMetadataGenerator[Unit] {
    override def generate(a: Unit)(v: ActionMetadata): ActionMetadata = v
  }
  private val executionEngine = new Engine[Unit, Unit]
  private val seqId1 = seqId
  private val seqId2 = "TEST-02"
  private val seqId3 = "TEST-03"
  private val qs2 = Engine.State[Unit]((), qs1.sequences + (seqId2 -> qs1.sequences(seqId1)))
  private val qs3 = Engine.State[Unit]((), qs2.sequences + (seqId3 -> seqG))
  private val user = UserDetails("telops", "Telops")

  def isFinished(status: SequenceState): Boolean = status match {
    case SequenceState.Idle      => true
    case SequenceState.Completed => true
    case SequenceState.Failed(_) => true
    case _                       => false
  }

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def runToCompletion(s0: Engine.State[Unit]): Option[Engine.State[Unit]] = {
    executionEngine.process(Stream.eval(IO.pure(Event.start(seqId, user, UUID.randomUUID()))))(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunSync.map(_._2)
  }

  it should "be in Running status after starting" in {
    val p = Stream.eval(IO.pure(Event.start(seqId, user, UUID.randomUUID())))
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

  private def actionPause: Option[Engine.State[Unit]] = {
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(
            id = 1,
            fileId = None,
            config = config,
            resources = Set(GmosS),
            executions = List(
              List(fromIO(ActionType.Undefined,
                IO(Result.Paused(new Result.PauseContext {}))))
            )
          )
        )
      ) ) ) )
    )
    val p = Stream.eval(IO.pure(Event.start(seqId, user, UUID.randomUUID())))

    executionEngine.process(p)(s0).take(1).compile.last.unsafeRunSync.map(_._2)
  }

  "sequence state" should "stay as running when action pauses itself" in {
    assert(actionPause.exists(s => Sequence.State.isRunning(s.sequences(seqId))))
  }

  ignore should "change to Paused if output is Paused" in {
    assert(actionPause.exists(_.sequences(seqId).current.execution.forall(Action.paused)))
  }

  "engine" should "run sequence to completion after resuming a paused action" in {
    val p = Stream.eval(IO.pure(Event.actionResume(seqId, 0, IO(Result.OK(Result.Configured(GmosS))))))

    val result = actionPause.flatMap(executionEngine.process(p)(_).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunTimed(5.seconds))
    val qso = result.flatMap(_.map(_._2))

    assert(qso.forall(x => x.sequences(seqId).current.actions.isEmpty && (x.sequences(seqId).status === SequenceState.Completed)))

  }

  it should "not run 2nd sequence because it's using the same resource" in {
    val p = Stream.emits(List(Event.start(seqId1, user, UUID.randomUUID()), Event.start(seqId2, user, UUID.randomUUID()))).evalMap(IO.pure(_))
    assert(
      executionEngine.process(p)(qs2).take(6).compile.last.unsafeRunSync.map(_._2.sequences(seqId2)).exists(_.status === SequenceState.Idle)
    )
  }

  it should "run 2nd sequence when there are no shared resources" in {
    val p = Stream.emits(List(Event.start(seqId1, user, UUID.randomUUID()), Event.start(seqId3, user, UUID.randomUUID()))).evalMap(IO.pure(_))

    assert(
      executionEngine.process(p)(qs3).take(6).compile.last.unsafeRunSync.exists(t => Sequence.State.isRunning(t._2.sequences(seqId3)))
    )
  }

  "engine" should "keep processing input messages regardless of how long Actions take" in {
    val result = (for {
      q <- Stream.eval(async.boundedQueue[IO, executionEngine.EventType](10))
      startedFlag <- Stream.eval(Semaphore.apply[IO](0))
      finishFlag <- Stream.eval(Semaphore.apply[IO](0))
    } yield {
      val qs = Engine.State[Unit]((),
        Map((seqId, Sequence.State.init(Sequence(
          id = "First",
          metadata = SequenceMetadata(GmosS, None, ""),
          steps = List(
            Step.init(
              id = 1,
              fileId = None,
              config = config,
              resources = Set(GmosS),
              executions = List(
                List(fromIO(ActionType.Configure(TCS),
                  IO.apply {
                    startedFlag.decrement
                    finishFlag.increment
                    Result.OK(Result.Configured(TCS))
                  }))
              )
            )
          )
        ))))
      )
      List(
        List[IO[Unit]](
          q.enqueue1(Event.start(seqId, user, UUID.randomUUID())),
          startedFlag.increment,
          q.enqueue1(Event.getState[executionEngine.ConcreteTypes] { _ => finishFlag.decrement *> IO.apply(None) })
        ).sequence,
        executionEngine.process(q.dequeue)(qs).drop(1).takeThrough(a => !isFinished(a._2.sequences(seqId).status)).compile.drain
      ).parSequence
    }).attempt.compile.last.unsafeRunTimed(5.seconds).flatten

    assert(result.forall(_.isRight))
  }

  "engine" should "not capture fatal errors." in {
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def s0(e: Throwable): Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(
            id = 1,
            fileId = None,
            config = config,
            resources = Set(GmosS),
            executions = List(
              List(fromIO(ActionType.Undefined,
                IO.apply {
                  throw e
                }))
            )
          )
        )
      ) ) ) )
    )

    intercept[RuntimeException](
      runToCompletion(s0(new RuntimeException))
    )
    // TODO: Restore these tests, see SEQNG-586
   // intercept[OutOfMemoryError](
   //   runToCompletion(s0(new OutOfMemoryError))
   // )
//    intercept[StackOverflowError](
//      runToCompletion(s0(new StackOverflowError))
//    )
  }

  case class DummyData(operator: Option[Operator])
  implicit object DummyDataCanGenerateActionMetadata extends ActionMetadataGenerator[DummyData] {
    override def generate(a: DummyData)(v: ActionMetadata): ActionMetadata = v.copy(operator = a.operator)
  }
  private val executionEngine2 = new Engine[DummyData, Unit]
  case class DummyResult(operator: Option[Operator]) extends Result.RetVal
  private val operatorL: Lens[DummyData, Option[Operator]] = GenLens[DummyData](_.operator)

  "engine" should "pass parameters to Actions." in {

    def setOperator(op: Operator): executionEngine2.StateType => executionEngine2.StateType = (Engine.State.userDataL[DummyData] ^|-> operatorL).set(op.some)

    val opName = Operator("John")

    val p = Stream.emits(List(Event.modifyState[executionEngine2.ConcreteTypes](setOperator(opName), ()), Event.start(seqId1, user, UUID.randomUUID()))).evalMap(IO.pure(_))
    val s0 = Engine.State[DummyData](DummyData(None),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(
            id = 1,
            fileId = None,
            config = config,
            resources = Set(GmosS),
            executions = List(
              List(Action(ActionType.Undefined, Kleisli(v => IO(Result.OK(DummyResult(v.operator)))), Action.State(Action.Idle, Nil)))
            )
          )
        )
      ) ) ) )
    )

    val sf = executionEngine2.process(p)(s0).drop(3).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).compile.last.unsafeRunSync.map(_._2)

    assertResult(Some(Action.Completed(DummyResult(Some(opName))))){
      for {
        st <- sf
        sq <- st.sequences.get(seqId)
        st <- sq.done.headOption
        as <- st.executions.headOption
        ac <- as.headOption
      } yield ac.state.runState
    }
  }

  it should "skip steps marked to be skipped at the beginning of the sequence." in {
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions),
          Step.init(id = 3, fileId = None, config = config, resources = Set(GmosS), executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped, StepState.Completed, StepState.Completed))
    }
  }

  it should "skip steps marked to be skipped in the middle of the sequence." in {
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, fileId = None, config = config, resources = Set(GmosS), executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip several steps marked to be skipped." in {
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 4, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 5, fileId = None, config = config, resources = Set(GmosS), executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped, StepState.Skipped, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip steps marked to be skipped at the end of the sequence." in {
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions),
          Step.init(id = 3, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true))
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
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true))
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped))
    }
  }

  it should "skip steps marked to be skipped at the beginning of the sequence, even if they have breakpoints." in {
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 3, fileId = None, config = config, resources = Set(GmosS), executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Skipped, StepState.Skipped, StepState.Completed))
    }
  }

  it should "skip the leading steps if marked to be skipped, even if they have breakpoints and are the last ones." in {
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipped = Step.Skipped(true)),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true),
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
    val s0: Engine.State[Unit] = Engine.State[Unit]((),
      Map((seqId, Sequence.State.init(Sequence(
        id = "First",
        metadata = SequenceMetadata(GmosS, None, ""),
        steps = List(
          Step.init(id = 1, fileId = None, config = config, resources = Set(GmosS), executions = executions),
          Step.init(id = 2, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true)),
          Step.init(id = 3, fileId = None, config = config, resources = Set(GmosS), executions = executions).copy(skipMark = Step.SkipMark(true),
            breakpoint = Step.BreakpointMark(true)),
          Step.init(id = 4, fileId = None, config = config, resources = Set(GmosS), executions = executions)
        )
      ) ) ) )
    )

    val sf = runToCompletion(s0)

    inside (sf.map(_.sequences(seqId).done.map(Step.status))) {
      case Some(stepSs) => assert(stepSs === List(StepState.Completed, StepState.Skipped))
    }
  }

}
