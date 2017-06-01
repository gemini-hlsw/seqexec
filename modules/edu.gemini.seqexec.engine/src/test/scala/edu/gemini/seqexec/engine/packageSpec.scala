package edu.gemini.seqexec.engine

import java.util.concurrent.Semaphore

import Event._
import org.scalatest.FlatSpec

import edu.gemini.seqexec.model.Model.SequenceState.{Error, Idle}
import edu.gemini.seqexec.model.Model.{Conditions, SequenceMetadata, SequenceState, StepConfig}

import scala.concurrent.duration._
import scalaz.syntax.apply._
import scalaz.syntax.foldable._
import scalaz.Nondeterminism
import scalaz.std.AllInstances._
import scalaz.concurrent.Task
import scalaz.stream.Cause
import scalaz.stream.async

class packageSpec extends FlatSpec {

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action  = for {
    _ <- Task(Thread.sleep(200))
  } yield Result.OK(Result.Configured("TCS"))

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = for {
    _ <- Task(Thread.sleep(200))
  } yield Result.OK(Result.Configured("Instrument"))

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = for {
    _ <- Task(Thread.sleep(200))
} yield Result.OK(Result.Observed("DummyFileId"))

  val faulty: Action  = for {
    _ <- Task(Thread.sleep(100))
  } yield Result.Error("There was an error in this action")

  val config: StepConfig = Map()
  val seqId = "TEST-01"
  val qs1: Engine.State =
    Engine.State(
      Conditions.default,
      None,
      Map(
        (seqId,
         Sequence.State.init(
           Sequence(
             "First",
             SequenceMetadata("F2", None, ""),
             List(
               Step(
                 1,
                 None,
                 config,
                 Set(Resource.TCS, Resource.F2),
                 breakpoint = false,
                 List(
                   List(configureTcs, configureInst), // Execution
                   List(observe) // Execution
                 )
               ),
               Step(
                 2,
                 None,
                 config,
                 Set(Resource.TCS, Resource.OI, Resource.F2),
                 breakpoint = false,
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

  val seqG =
    Sequence.State.init(
      Sequence(
        "First",
        SequenceMetadata("GMOS", None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(Resource.GMOS),
            breakpoint = false,
            List(
              List(configureTcs, configureInst), // Execution
              List(observe) // Execution
            )
          )
        )
      )
    )

  val seqId1 = seqId
  val seqId2 = "TEST-02"
  val seqId3 = "TEST-03"
  val qs2 = Engine.State(Conditions.default, None, qs1.sequences + (seqId2 -> qs1.sequences(seqId1)))
  val qs3 = Engine.State(Conditions.default, None, qs2.sequences + (seqId3 -> seqG))

  def isFinished(status: SequenceState): Boolean =
    status == Idle || status == edu.gemini.seqexec.model.Model.SequenceState.Completed || status === Error

  def runToCompletion(q: scalaz.stream.async.mutable.Queue[Event], s0: Engine.State): Engine.State = {

    (q.enqueueOne(start(seqId)) *>
      process(q, q.dequeue)(s0).drop(1).takeThrough(
        a => !isFinished(a._2.sequences(seqId).status)
      ).runLast
    ).unsafePerformSync.get._2

  }

  it should "be in Running status after starting" in {
    val q = async.boundedQueue[Event](10)
    val qs = (q.enqueueOne(start(seqId)) *> process(q, q.dequeue)(qs1).take(2).runLast).unsafePerformSync.get._2
    assert(qs.sequences(seqId).status === SequenceState.Running)
  }

  ignore should "be 0 pending executions after execution" in {
    val q = async.boundedQueue[Event](10)
    val qs = runToCompletion(q, qs1)
    assert(qs.sequences(seqId).pending.isEmpty)
  }

  ignore should "be 2 Steps done after execution" in {
    val q = async.boundedQueue[Event](10)
    val qs = runToCompletion(q, qs1)
    assert(qs.sequences(seqId).done.length == 2)
  }

  ignore should "Print execution" in {
    val q = async.boundedQueue[Event](10)
    intercept[Cause.Terminated](
      Nondeterminism[Task].both(
        List(
          q.enqueueOne(start(seqId)),
          Task(Thread.sleep(5000)),
          q.enqueueOne(exit)
        ).sequence_,
        process(q, q.dequeue)(qs1).run
      ).unsafePerformSync
    )
  }

  ignore should "Print execution with pause" in {
    val q = async.boundedQueue[Event](10)
    intercept[Cause.Terminated](
      Nondeterminism[Task].both(
        List(
          q.enqueueOne(start(seqId)),
          Task(Thread.sleep(2000)),
          q.enqueueOne(pause(seqId)),
          Task(Thread.sleep(1000)),
          q.enqueueOne(start(seqId)),
          Task(Thread.sleep(5000)),
          q.enqueueOne(exit)
        ).sequence_,
       process(q, q.dequeue)(qs1).run
      ).unsafePerformSync
    )
  }

  ignore should "not run 2nd sequence because it's using the same resource" in {
    val q = async.boundedQueue[Event](10)

    assert(
      Nondeterminism[Task].both(
        q.enqueueOne(start(seqId1)) *> q.enqueueOne(start(seqId2)),
        process(q, q.dequeue)(qs2).take(6).runLast
      ).unsafePerformSync._2.get._2.sequences(seqId2).status === SequenceState.Idle
    )
  }

  ignore should "run 2nd sequence when there are no shared resources" in {
    val q = async.boundedQueue[Event](10)

    assert(
      Nondeterminism[Task].both(
        q.enqueueOne(start(seqId1)) *> q.enqueueOne(start(seqId3)),
        process(q, q.dequeue)(qs3).take(6).runLast
      ).unsafePerformSync._2.get._2.sequences(seqId3).status === SequenceState.Running
    )
  }

  ignore should "keep processing input messages regardless of how long Actions take" in {
    val q = async.boundedQueue[Event](10)
    val startedFlag = new Semaphore(0)
    val finishFlag = new Semaphore(0)

    val qs = Engine.State(Conditions.default,
      None,
      Map((seqId, Sequence.State.init(Sequence(
        "First",
        SequenceMetadata("GMOS", None, ""),
        List(
          Step(
            1,
            None,
            config,
            Set(Resource.GMOS),
            breakpoint = false,
            List(
              List(Task.apply{
                startedFlag.release
                finishFlag.acquire
                Result.OK(Result.Configured("TCS"))
              } )
            )
          )
        )
      ) ) ) )
    )

    val result = Nondeterminism[Task].both(
        q.enqueueOne(start(seqId)) *> Task.apply(startedFlag.acquire) *>
          q.enqueueOne(Event.getState{_ => Task.delay{finishFlag.release}}),
        process(q, q.dequeue)(qs).drop(1).takeThrough(a => !isFinished(a._2.sequences(seqId).status)).run
      ).timed(5.seconds).unsafePerformSyncAttempt
    assert(result.isRight)
  }
}
