package edu.gemini.seqexec.engine

import Result._
import Event._
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
    _ <- Task(Thread.sleep(200))
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

  val faulty: Action  = for {
    _ <- Task(println("System: Start observation"))
    _ <- Task(Thread.sleep(100))
    _ <- Task(println ("System: Complete observation"))
  } yield Result.Error(Unit)

  val config: StepConfig = Map()
  val seqId ="TEST-01"
  val qs1: EngineState = Map((seqId, Sequence.State.init(
    Sequence(
      "First",
      SequenceMetadata("F2"),
      List(
        Step(
          1,
          None,
          config,
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
          false,
          List(
            List(configureTcs, configureInst), // Execution
            List(observe) // Execution
          )
        )
      )
    )
  )))

  it should "be in Running status after starting" in {
    val q = async.boundedQueue[Event](10)
    val qs = (q.enqueueOne(start(seqId)) *> processE(q).take(1).runLast.eval(qs1)).unsafePerformSync.get._2
    assert(qs(seqId).status === SequenceState.Running)
  }

  it should "be 0 pending executions after execution" in {
    val q = async.boundedQueue[Event](10)
    val qs = (
      q.enqueueOne(start(seqId)) *>
        // 6 Actions + 4 Nexts + 4 Executing + 4 Executed + 1 start + 1 finished => take(20)
        processE(q).take(20).runLast.eval(qs1)).unsafePerformSync.get._2
    assert(qs(seqId).pending.isEmpty)
  }

  it should "be 2 Steps done after execution" in {
    val q = async.boundedQueue[Event](10)
    val qs = (
      q.enqueueOne(start(seqId)) *>
        // 6 Actions + 4 Nexts + 4 Executing + 4 Executions + 1 start + 1 finished => take(20)
        processE(q).take(20).runLast.eval(qs1)).unsafePerformSync.get._2
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
}
