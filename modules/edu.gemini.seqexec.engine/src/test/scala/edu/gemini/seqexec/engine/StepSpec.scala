package edu.gemini.seqexec.engine

import edu.gemini.seqexec.model.Model.{SequenceMetadata, SequenceState, StepConfig, StepState}

import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.Conditions

import scalaz.syntax.either._
import org.scalatest._
import Matchers._
import Inside._

import scalaz.concurrent.Task
import scalaz.stream.async
import edu.gemini.seqexec.engine.Event.{pause, start}
import edu.gemini.seqexec.model.Model.SequenceState.{Error, Idle, Running}

import scala.Function.const
import scalaz.\/-


/**
  * Created by jluhrs on 9/29/16.
  */
class StepSpec extends FlatSpec {

  val seqId ="TEST-01"

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action  = for {
    _ <- Task(println("System: Start TCS configuration"))
    _ <- Task(Thread.sleep(200))
    _ <- Task(println ("System: Complete TCS configuration"))
  } yield Result.OK(Result.Configured("TCS"))

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = for {
    _ <- Task(println("System: Start Instrument configuration"))
    _ <- Task(Thread.sleep(150))
    _ <- Task(println("System: Complete Instrument configuration"))
  } yield Result.OK(Result.Configured("Instrument"))

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = for {
    _ <- Task(println("System: Start observation"))
    _ <- Task(Thread.sleep(200))
    _ <- Task(println ("System: Complete observation"))
  } yield Result.OK(Result.Observed("DummyFileId"))

  def triggerPause(q: async.mutable.Queue[Event]): Action = for {
    _ <- q.enqueueOne(pause(seqId))
    // There is not a distinct result for Pause because the Pause action is a
    // trick for testing but we don't need to support it real life, he pause
    // input event is enough.
  } yield Result.OK(Result.Observed("DummyFileId"))

  def runToCompletion(q: scalaz.stream.async.mutable.Queue[Event], s0: Engine.State): Engine.State = {
    def isFinished(status: SequenceState): Boolean =
      status == Idle || status == edu.gemini.seqexec.model.Model.SequenceState.Completed || status === Error

    q.enqueueOne(start(seqId)).flatMap(
      _ => processE(q).drop(1).takeThrough(
        a => !isFinished(a._2.sequences.get(seqId).get.status)
      ).runLast.eval(s0)
    ).unsafePerformSync.get._2
  }

  def runToCompletionL(q: scalaz.stream.async.mutable.Queue[Event], s0: Engine.State): List[Engine.State] = {
    def isFinished(status: SequenceState): Boolean =
      status == Idle || status == edu.gemini.seqexec.model.Model.SequenceState.Completed || status === Error

    q.enqueueOne(start(seqId)).flatMap(
      _ => processE(q).drop(1).takeThrough(
        a => !isFinished(a._2.sequences.get(seqId).get.status)
      ).runLog.eval(s0)
    ).unsafePerformSync.map(_._2).toList

  }

  // This test must have a simple step definition and the known sequence of updates that running that step creates.
  // The test will just run step and compare the output with the predefined sequence of updates.
  ignore should "run and generate the predicted sequence of updates." in {

  }

  // The difficult part is to set the pause command to interrupts the step execution in the middle.
  "pause" should "stop execution in response to a pause command" in {
    val q = async.boundedQueue[Event](10)
    val qs0: Engine.State =
      Engine.State(
        Conditions.WORST,
        Map(
          (seqId,
           Sequence.State.init(
             Sequence(
               seqId,
               SequenceMetadata("F2", None, None),
               List(
                 Step(
                   1,
                   None,
                   config,
                   Set.empty,
                   false,
                   List(
                     List(configureTcs, configureInst, triggerPause(q)), // Execution
                     List(observe) // Execution
                   )
                 )
               )
             )
           )
          )
        )
      )

    val qs1 = runToCompletion(q, qs0)

     inside (qs1.sequences.get(seqId).get) {
      case Sequence.State.Zipper(zipper, status) =>
        status should be (Idle)
        inside (zipper.focus.toStep) {
          case Step(_, _, _, _, _, ex1::ex2::Nil) =>
            assert( Execution(ex1).results.length == 3 && Execution(ex2).actions.length == 1)

        }
    }

  }

  it should "resume execution from the non-running state in response to a resume command, rolling back a partially run step." in {
    val q = async.boundedQueue[Event](10)
    // Engine state with one idle sequence partially executed. One Step completed, two to go.
    val qs0: Engine.State =
      Engine.State(
        Conditions.WORST,
        Map(
          (seqId,
           Sequence.State.Zipper(
             Sequence.Zipper(
               "First",
               SequenceMetadata("F2", None, None),
               Nil,
               Step.Zipper(
                 2,
                 None,
                 config,
                 Set.empty,
                 false,
                 Nil,
                 Execution(List(observe.left)),
                 List(List(result, result)),
                 (Execution(List(configureTcs.left, configureInst.left)), List(List(observe)))),
               Nil
             ),
             Idle
           )
          )
        )
      )

    val qs1 = (
      q.enqueueOne(start(seqId)).flatMap(_ =>
        processE(q).take(1).runLast.eval(qs0)
      )
    ).unsafePerformSync.get._2

    inside (qs1.sequences.get(seqId).get) {
      case Sequence.State.Zipper(zipper, status) =>
        status should be (Running)
        inside (zipper.focus.toStep) {
          case Step(_, _, _, _, _, ex1::ex2::Nil) =>
            assert(Execution(ex1).actions.length == 2 && Execution(ex2).actions.length == 1)
        }
    }

  }

  ignore should "ignore pause command if step is not been executed." in {

  }

  // Be careful that resume command really arrives while sequence is running.
  ignore should "ignore resume command if step is already running." in {

  }

  // For this test, one of the actions in the step must produce an error as result.
  ignore should "stop execution and propagate error when an Action ends in error." in {

  }

  "engine" should "record a partial result and continue execution." in {

    // For result types
    case class RetValDouble(v: Double) extends Result.RetVal
    case class PartialValDouble(v: Double) extends Result.PartialVal

    val q = async.boundedQueue[Event](10)
    val qs0: Engine.State =
      Engine.State(
        Conditions.WORST,
        Map(
          (seqId,
           Sequence.State.init(
             Sequence(
               seqId,
               SequenceMetadata("F2", None, None),
               List(
                 Step(
                   1,
                   None,
                   config,
                   Set.empty,
                   false,
                   List(
                     List(
                       Task(
                         Result.Partial(
                           PartialValDouble(0.5),
                           Task(Result.OK(RetValDouble(1.0))
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
        )
      )

    val qss = runToCompletionL(q, qs0)

    inside (qss.tail.head.sequences.get(seqId).get) {
      case Sequence.State.Zipper(zipper, status) =>
        status shouldBe SequenceState.Running
        inside (zipper.focus.focus.execution.head) {
          case \/-(Result.Partial(v, _)) => v shouldEqual PartialValDouble(0.5)
        }
    }
    inside (qss.last.sequences.get(seqId).get) {
      case Sequence.State.Final(seq, status) =>
        status shouldBe SequenceState.Completed
        seq.steps.head.executions.head.head shouldEqual Result.OK(RetValDouble(1.0))
    }

  }

  val result = Result.OK(Result.Observed("dummyId"))
  val failure = Result.Error("Dummy error")
  val action: Action = Task(result)
  val config: StepConfig = Map.empty
  def simpleStep(pending: List[Actions], focus: Execution, done: List[Results]): Step.Zipper = {
    val rollback: (Execution, List[Actions]) = (done.map(_.map(const(action))) ++ List(focus.execution.map(const(action))) ++ pending) match {
      case Nil => (Execution.empty, Nil)
      case x::xs => (Execution(x.map(_.left)), xs)
    }

    Step.Zipper(1, None, config, Set.empty, false, pending, focus, done, rollback)
  }

  val stepz0: Step.Zipper   = simpleStep(Nil, Execution.empty, Nil)
  val stepza0: Step.Zipper  = simpleStep(List(List(action)), Execution.empty, Nil)
  val stepza1: Step.Zipper  = simpleStep(List(List(action)), Execution(List(result.right)), Nil)
  val stepzr0: Step.Zipper  = simpleStep(Nil, Execution.empty, List(List(result)))
  val stepzr1: Step.Zipper  = simpleStep(Nil, Execution(List(result.right, result.right)), Nil)
  val stepzr2: Step.Zipper  = simpleStep(Nil, Execution(List(result.right, result.right)), List(List(result)))
  val stepzar0: Step.Zipper = simpleStep(Nil, Execution(List(result.right, action.left)), Nil)
  val stepzar1: Step.Zipper = simpleStep(List(List(action)), Execution(List(result.right, result.right)), List(List(result)))

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

  val step0: Step[Action] = Step(1, None, config, Set.empty, false, List(Nil))
  val step1: Step[Action] = Step(1, None, config, Set.empty, false, List(List(action)))
  val step2: Step[Action] = Step(2, None, config, Set.empty, false, List(List(action, action), List(action)))

  "currentify" should "be None only when a Step is empty of executions" in {
    assert(Step.Zipper.currentify(Step(0, None, config, Set.empty, false, Nil)).isEmpty)
    assert(Step.Zipper.currentify(step0).isEmpty)
    assert(Step.Zipper.currentify(step1).nonEmpty)
    assert(Step.Zipper.currentify(step2).nonEmpty)
  }

  "status" should "be Error when empty" in {
    assert(Step.status(stepz0.toStep) === StepState.Error(
             "This should never happen, please submit a bug report"
           )
    )
  }

  "status" should "be Error when at least one Action failed" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          None,
          Map.empty,
          Set.empty,
          false,
          Nil,
          Execution(List(action.left, failure.right, result.right)),
          Nil,
          (Execution(List(action.left, action.left, action.left)), Nil)
        ).toStep
      ) === StepState.Error("Dummy error")
    )
  }

  "status" should "be Completed when all actions succeeded" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          None,
          Map.empty,
          Set.empty,
          false,
          Nil,
          Execution(List(result.right, result.right, result.right)),
          Nil,
          (Execution(List(action.left, action.left, action.left)), Nil)
        ).toStep
      ) === StepState.Completed
    )
  }

  "status" should "be Running when there are both actions and results" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          None,
          Map.empty,
          Set.empty,
          false,
          Nil,
          Execution(List(result.right, action.left, result.right)),
          Nil,
          (Execution(List(action.left, action.left, action.left)), Nil)
        ).toStep
      ) === StepState.Running
    )
  }

  "status" should "be Pending when there are only pending actions" in {
    assert(
      Step.status(
        Step.Zipper(
          1,
          None,
          Map.empty,
          Set.empty,
          false,
          Nil,
          Execution(List(action.left, action.left, action.left)),
          Nil,
          (Execution(List(action.left, action.left, action.left)), Nil)
        ).toStep
      ) === StepState.Pending
    )
  }

}
