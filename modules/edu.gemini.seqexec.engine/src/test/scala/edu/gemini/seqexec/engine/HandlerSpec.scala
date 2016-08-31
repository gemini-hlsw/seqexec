package edu.gemini.seqexec.engine

import scala.collection.immutable.IntMap

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
  } yield OK

  val qs1: QueueStatus = QueueStatus(
    Queue(
      List(
        List(
          List(
            List(configureTcs, configureInst),
            List(observe)
          ),
          List()
        ),
        List(
          List(
            List(configureTcs, configureInst),
            List(observe)
          ),
          List()
        )
      ),
      IntMap(),
        List()
    ),
    Status.Waiting
  )

  val emptyQueueStatus: QueueStatus =
    QueueStatus(Queue(List(), IntMap(), List()), Status.Waiting)

  val queue = async.boundedQueue[Event](10)

  // Start the engine with an initial `QueueStatus` and collect the final `QueueStatus`
  // def topple(qs: QueueStatus): Task[QueueStatus] =
  //   queue.enqueueOne(start) *> Handler.run(queue)(qs)

  // it should "be in running Status after a Start event" in {
  //   val result = topple(emptyQueueStatus).unsafePerformSync
  //   assert(result.status === Status.Running)
  // }

  // it should "No pending sequences after running qs1" in {
  //   val result = topple(qs1).unsafePerformSync
  //   assert(QueueStatus.pendingExecution.get(result) === Some(List()))
  // }

  // This is here for visual inspection. The states should be coupled with a //
  // Sink for concrete tests.
  // it should "0 steps pending after pause/start events" in {
  //   val result =
  //     (queue.enqueueAll(List(start, poll, pause, poll, start, poll)) *>
  //        (handler(queue).take(9).run.exec(qs1))).unsafePerformSync
  //   assert(result.queue.pending.length == 2)
  // }

  it should "finish raising a Terminated exception after an Exit event" in {
    intercept[Cause.Terminated](Nondeterminism[Task].both(
      (queue.enqueueOne(start) *> queue.enqueueOne(exit)),
      (handler(queue).run.exec(qs1))
      ).unsafePerformSync
    )
  }
}
