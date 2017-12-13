// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import java.util.concurrent.Semaphore

import org.scalatest.{FlatSpec, NonImplicitAssertions}
import edu.gemini.seqexec.model.Model.{Conditions, SequenceMetadata, SequenceState, StepConfig}
import edu.gemini.seqexec.model.Model.{Observer, Operator, Resource}
import edu.gemini.seqexec.model.Model.Instrument.{F2, GmosS}
import edu.gemini.seqexec.model.Model.Resource.TCS
import edu.gemini.seqexec.model.{ActionType, UserDetails}

import scala.concurrent.duration._
import scalaz._
import Scalaz._
import scalaz.Nondeterminism
import scalaz.concurrent.Task
import scalaz.stream.{Process, async}

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class packageSpec extends FlatSpec with NonImplicitAssertions {

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action  = fromTask(ActionType.Configure(TCS),
    for {
      _ <- Task(Thread.sleep(200))
    } yield Result.OK(Result.Configured(TCS)))

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = fromTask(ActionType.Configure(GmosS),
    for {
    _ <- Task(Thread.sleep(200))
  } yield Result.OK(Result.Configured(GmosS)))

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = fromTask(ActionType.Observe,
  for {
    _ <- Task(Thread.sleep(200))
  } yield Result.OK(Result.Observed("DummyFileId")))

  val faulty: Action  = fromTask(ActionType.Undefined,
  for {
    _ <- Task(Thread.sleep(100))
  } yield Result.Error("There was an error in this action"))

  val config: StepConfig = Map()
  val seqId: String = "TEST-01"
  val qs1: Engine.State =
    Engine.State(
      Conditions.default,
      None,
      Map(
        (seqId,
         Sequence.State.init(
           Sequence(
             "First",
             SequenceMetadata(F2, None, ""),
             List(
               Step(
                 1,
                 None,
                 config,
                 Set(Resource.TCS, F2),
                 breakpoint = false,
                 skip = false,
                 List(
                   List(configureTcs, configureInst), // Execution
                   List(observe) // Execution
                 )
               ),
               Step(
                 2,
                 None,
                 config,
                 Set(Resource.TCS, Resource.OI, F2),
                 breakpoint = false,
                 skip = false,
                 List(
                   List(configureTcs, configureInst), // Execution
                   List(observe) // Execution
                 )
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
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(GmosS),
            breakpoint = false,
            skip = false,
            List(
              List(configureTcs, configureInst), // Execution
              List(observe) // Execution
            )
          )
        )
      )
    )

  private val seqId1 = seqId
  private val seqId2 = "TEST-02"
  private val seqId3 = "TEST-03"
  private val qs2 = Engine.State(Conditions.default, None, qs1.sequences + (seqId2 -> qs1.sequences(seqId1)))
  private val qs3 = Engine.State(Conditions.default, None, qs2.sequences + (seqId3 -> seqG))
  private val user = UserDetails("telops", "Telops")

  def isFinished(status: SequenceState): Boolean = status match {
    case SequenceState.Idle      => true
    case SequenceState.Completed => true
    case SequenceState.Error(_)  => true
    case _                       => false
  }

  def runToCompletion(s0: Engine.State): Option[Engine.State] = {
    process(Process.eval(Task.now(Event.start(seqId, user))))(s0).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).runLast.unsafePerformSync.map(_._2)
  }

  it should "be in Running status after starting" in {
    val p = Process.eval(Task.now(Event.start(seqId, user)))
    val qs = process(p)(qs1).take(1).runLast.unsafePerformSync.map(_._2)
    assert(qs.map(_.sequences(seqId).status === SequenceState.Running).getOrElse(false))
  }

  it should "be 0 pending executions after execution" in {
    val qs = runToCompletion(qs1)
    assert(qs.map(_.sequences(seqId).pending.isEmpty).getOrElse(false))
  }

  it should "be 2 Steps done after execution" in {
    val qs = runToCompletion(qs1)
    assert(qs.map(_.sequences(seqId).done.length === 2).getOrElse(false))
  }

  private def actionPause: Option[Engine.State] = {
    val s0: Engine.State = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(GmosS),
            breakpoint = false,
            skip = false,
            List(
              List(fromTask(ActionType.Undefined,
              Task(Result.Paused)))
            )
          )
        )
      ) ) ) )
    )
    val p = Process.eval(Task.now(Event.start(seqId, user)))

    process(p)(s0).runLast.unsafePerformSync.map(_._2)
  }

  "sequence state" should "stay as running when action pauses itself" in {
    assert(actionPause.map(_.sequences(seqId).status === SequenceState.Running).getOrElse(false))
  }

  "action state" should "change to Paused if output is Paused" in {
    assert(actionPause.map(_.sequences(seqId).current.execution.forall(_.state.runState === Action.Paused)).getOrElse(false))
  }

  "engine" should "run sequence to completion after resuming a paused action" in {
    val p = Process.eval(Task.now(Event.actionResume(seqId, 0, Task(Result.OK(Result.Configured(GmosS))))))

    val result = actionPause.map(process(p)(_).drop(1).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).runLast.timed(5.seconds).unsafePerformSyncAttempt)
    val qso = result.map(_.map(_.map(_._2)))

    assert(qso.map(qs => qs.isRight && qs.forall(x => x.isDefined && x.map(_.sequences(seqId).current.actions.isEmpty).getOrElse(false) &&
      x.map(_.sequences(seqId).status === SequenceState.Completed).getOrElse(false))).getOrElse(false))

  }

  it should "not run 2nd sequence because it's using the same resource" in {
    val p = Process.emitAll(List(Event.start(seqId1, user), Event.start(seqId2, user))).evalMap(Task.now(_))
    assert(
      process(p)(qs2).take(6).runLast.unsafePerformSync.map(_._2.sequences(seqId2)).map(_.status === SequenceState.Idle)getOrElse(false)
    )
  }

  it should "run 2nd sequence when there are no shared resources" in {
    val p = Process.emitAll(List(Event.start(seqId1, user), Event.start(seqId3, user))).evalMap(Task.now(_))

    assert(
      process(p)(qs3).take(6).runLast.unsafePerformSync.map(_._2.sequences(seqId3).status === SequenceState.Running).getOrElse(false)
    )
  }

  "engine" should "keep processing input messages regardless of how long Actions take" in {
    val q = async.boundedQueue[Event](10)
    val startedFlag = new Semaphore(0)
    val finishFlag = new Semaphore(0)

    val qs = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(GmosS),
            breakpoint = false,
            skip = false,
            List(
              List(fromTask(ActionType.Configure(TCS),
              Task.apply{
                startedFlag.release()
                finishFlag.acquire()
                Result.OK(Result.Configured(TCS))
              }) )
            )
          )
        )
      ) ) ) )
    )

    val result = Nondeterminism[Task].both(
      List(
        q.enqueueOne(Event.start(seqId, user)),
        Task.apply(startedFlag.acquire()),
        q.enqueueOne(Event.getState{_ => Task.delay{finishFlag.release()} *> Task.delay(None)})
      ).sequenceU,
        process(q.dequeue)(qs).drop(1).takeThrough(a => !isFinished(a._2.sequences(seqId).status)).run
      ).timed(5.seconds).unsafePerformSyncAttempt
    assert(result.isRight)
  }

  "engine" should "not capture fatal errors." in {
    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def s0(e: Error): Engine.State = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(GmosS),
            breakpoint = false,
            skip = false,
            List(
              List(fromTask(ActionType.Undefined,
              Task.apply{
                throw e
              }))
            )
          )
        )
      ) ) ) )
    )

    intercept[OutOfMemoryError](
      runToCompletion(s0(new OutOfMemoryError))
    )
    intercept[StackOverflowError](
      runToCompletion(s0(new StackOverflowError))
    )
  }

  "engine" should "pass parameters to Actions." in {
    val p = Process.emitAll(List(Event.setOperator(Operator("John"), user), Event.setObserver(seqId1, user, Observer("Smith")), Event.start(seqId1, user))).evalMap(Task.now(_))
    val s0 = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata(GmosS, None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(GmosS),
            breakpoint = false,
            skip = false,
            List(
              List(Action(ActionType.Undefined, Kleisli(v => Task(Result.OK(Result.Configured(TCS)))), Action.State(Action.Idle, List())))
            )
          )
        )
      ) ) ) )
    )

    val sf = process(p)(s0).drop(3).takeThrough(
      a => !isFinished(a._2.sequences(seqId).status)
    ).runLast.unsafePerformSync.map(_._2)

    assertResult(Some(Action.Completed(Result.Configured(TCS)))){
      for {
        st <- sf
        sq <- st.sequences.get(seqId)
        st <- sq.done.headOption
        as <- st.executions.headOption
        ac <- as.headOption
      } yield ac.state.runState
    }
  }

}
