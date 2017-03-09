package edu.gemini.seqexec.engine

import Result._
import Event._
import edu.gemini.seqexec.model.Model.SequenceState.{Idle, Error}
import edu.gemini.seqexec.model.Model.{SequenceMetadata, SequenceState, StepConfig}
import org.scalatest.FlatSpec

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
  val qs1: EngineState = Map((seqId, Sequence.State.init(
    Sequence(
      "First",
      SequenceMetadata("F2", None, None),
      List(
        Step(
          1,
          None,
          config,
          Set(Resource.Mount, Resource.F2),
          false,
          List(
            List(configureTcs, configureInst), // Execution
            List(observe) // Execution
          )
        ),
        Step(
          2,
          None,
          config,
          Set(Resource.Mount, Resource.OI, Resource.F2),
          false,
          List(
            List(configureTcs, configureInst), // Execution
            List(observe) // Execution
          )
        )
      )
    )
  )))

  val seqG =
    Sequence.State.init(
      Sequence(
        "First",
        SequenceMetadata("GMOS", None, None),
        List(
          Step(
            1,
            None,
            config,
            Set(Resource.GMOS),
            false,
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
  val qs2 = qs1 + (seqId2 -> qs1(seqId1))
  val qs3 = qs2 + (seqId3 -> seqG)

  def runToCompletion(q: scalaz.stream.async.mutable.Queue[Event], s0: EngineState): EngineState = {
    def isFinished(status: SequenceState): Boolean =
      status == Idle || status == edu.gemini.seqexec.model.Model.SequenceState.Completed || status === Error

    q.enqueueOne(start(seqId)).flatMap( _ =>
       processE(q).drop(1).takeThrough(a => !isFinished(a._2.get(seqId).get.status) ).runLast.eval(s0)).unsafePerformSync.get._2
  }


  it should "be in Running status after starting" in {
    val q = async.boundedQueue[Event](10)
    val qs = (q.enqueueOne(start(seqId)) *> processE(q).take(1).runLast.eval(qs1)).unsafePerformSync.get._2
    assert(qs(seqId).status === SequenceState.Running)
  }

  it should "be 0 pending executions after execution" in {
    val q = async.boundedQueue[Event](10)
    val qs = runToCompletion(q, qs1)
    assert(qs(seqId).pending.isEmpty)
  }

  it should "be 2 Steps done after execution" in {
    val q = async.boundedQueue[Event](10)
    val qs = runToCompletion(q, qs1)
    assert(qs(seqId).done.length == 2)
  }

  it should "Print execution" in {
    val q = async.boundedQueue[Event](10)
    intercept[Cause.Terminated](
      Nondeterminism[Task].both(
        List(
          q.enqueueOne(start(seqId)),
          Task(Thread.sleep(5000)),
          q.enqueueOne(exit)
        ).sequence_,
        processE(q).run.eval(qs1)
      ).unsafePerformSync
    )
  }

  it should "Print execution with pause" in {
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
       processE(q).run.eval(qs1)
      ).unsafePerformSync
    )
  }

  it should "not run 2nd sequence because it's using the same resource" in {
    val q = async.boundedQueue[Event](10)

    assert(
      Nondeterminism[Task].both(
        q.enqueueOne(start(seqId1)) *> q.enqueueOne(start(seqId2)),
        processE(q).take(6).runLast.eval(qs2)
      ).unsafePerformSync._2.get._2(seqId2).status === SequenceState.Idle
    )
  }

  it should "run 2nd sequence when there are no shared resources" in {
    val q = async.boundedQueue[Event](10)

    assert(
      Nondeterminism[Task].both(
        q.enqueueOne(start(seqId1)) *> q.enqueueOne(start(seqId3)),
        processE(q).take(6).runLast.eval(qs3)
      ).unsafePerformSync._2.get._2(seqId3).status === SequenceState.Running
    )
  }
}
