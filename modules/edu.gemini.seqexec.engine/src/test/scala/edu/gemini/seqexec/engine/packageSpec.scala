package edu.gemini.seqexec.engine

import Result._
import Event._
import edu.gemini.seqexec.model.Model.{SequenceMetadata, SequenceState, StepConfig}
import org.scalatest.FlatSpec

import scalaz._
import Scalaz._
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
    _ <- Task(Thread.sleep(2000))
    _ <- Task(println ("System: Complete TCS configuration"))
  } yield OK(())

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = for {
    _ <- Task(println("System: Start Instrument configuration"))
    _ <- Task(Thread.sleep(2000))
    _ <- Task(println("System: Complete Instrument configuration"))
  } yield OK(())

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = for {
    _ <- Task(println("System: Start observation"))
    _ <- Task(Thread.sleep(2000))
    _ <- Task(println ("System: Complete observation"))
  } yield OK(())

  val faulty: Action  = for {
    _ <- Task(println("System: Start observation"))
    _ <- Task(Thread.sleep(1000))
    _ <- Task(println ("System: Complete observation"))
  } yield Error(())

  val config: StepConfig = Map()
  val seqId ="TEST-01"
  val qs1: EngineState = Map((seqId, Sequence.State.init(
    Sequence(
      "First",
      SequenceMetadata("F2"),
      List(
        Step(
          1,
          config,
          List(
            List(configureTcs, configureInst), // Execution
            List(observe) // Execution
          )
        ),
        Step(
          2,
          config,
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
        // 6 Actions + 4 Executions + 1 start + 1 finished => take(12)
        processE(q).take(12).runLast.eval(qs1)).unsafePerformSync.get._2
    assert(qs(seqId).pending.isEmpty)
  }

  it should "be 1 Sequence done after execution" in {
    val q = async.boundedQueue[Event](10)
    val qs = (
      q.enqueueOne(start(seqId)) *>
        // 6 Actions + 4 Executions + 1 start + 1 finished => take(12)
        processE(q).take(12).runLast.eval(qs1)).unsafePerformSync.get._2
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
          Task(Thread.sleep(2000)),
          q.enqueueOne(start(seqId)),
          Task(Thread.sleep(3000)),
          q.enqueueOne(exit)
        ).sequence_,
       processE(q).run.eval(qs1)
      ).unsafePerformSync
    )
  }
}
