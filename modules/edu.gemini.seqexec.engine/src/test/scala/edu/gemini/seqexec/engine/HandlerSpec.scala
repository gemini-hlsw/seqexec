package edu.gemini.seqexec.engine

import Result._
import Event._
import Handler._
import org.scalatest.FlatSpec
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Cause
import scalaz.stream.async

class HandlerSpec extends FlatSpec {

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action  = for {
    _ <- Task(println("System: Start TCS configuration"))
    _ <- Task(Thread.sleep(2000))
    _ <- Task(println ("System: Complete TCS configuration"))
  } yield OK

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = for {
    _ <- Task(println("System: Start Instrument configuration"))
    _ <- Task(Thread.sleep(2000))
    _ <- Task(println("System: Complete Instrument configuration"))
  } yield OK

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = for {
    _ <- Task(println("System: Start observation"))
    _ <- Task(Thread.sleep(2000))
    _ <- Task(println ("System: Complete observation"))
  } yield OK

  val faulty: Action  = for {
    _ <- Task(println("System: Start observation"))
    _ <- Task(Thread.sleep(1000))
    _ <- Task(println ("System: Complete observation"))
  } yield Error

  val qs1: QState = QState.init(
    Queue(
      List(
        Sequence(
          "First",
          NonEmptyList(
            Step(
              1,
              NonEmptyList(
                NonEmptyList(configureTcs, configureInst), // Execution
                NonEmptyList(observe) // Execution
              )
            ),
            Step(
              2,
              NonEmptyList(
                NonEmptyList(configureTcs, configureInst), // Execution
                NonEmptyList(observe) // Execution
              )
            )
          )
        )
      )
    )
  )

  it should "Print qs1 execution" in {
    val q = async.boundedQueue[Event](10)
    intercept[Cause.Terminated](
      Nondeterminism[Task].both(
        q.enqueueOne(start),
        handler(q).run.exec(qs1)
      ).unsafePerformSync
    )
  }

  it should "Print qs1 execution with pause" in {
    val q = async.boundedQueue[Event](10)
    intercept[Cause.Terminated](
      Nondeterminism[Task].both(
        List(
          q.enqueueOne(start),
          Task(Thread.sleep(2000)),
          q.enqueueOne(pause),
          Task(Thread.sleep(2000)),
          q.enqueueOne(start)
        ).sequence_,
       handler(q).run.exec(qs1)
      ).unsafePerformSync
    )
  }
}
