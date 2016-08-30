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

  val emptySeqStatus: QueueStatus =
    QueueStatus(Queue(List(), IntMap(), List()), Status.Waiting)

  val queue = async.boundedQueue[Event](10)

  // Send an event and obtain a `SeqStatus`. This takes care of terminating the
  // process.
  def request(ev: Event): Task[QueueStatus] =
    queue.enqueueOne(start) *>
      (handler(queue).take(1).run.exec(emptySeqStatus))

  it should "be in running Status after a Start event" in {
    val result = request(start).unsafePerformSync.status
    assert(result === Status.Running)
  }

  // it should "0 steps pending after 4 steps have been processed for seqstatus1" in {
  //   val result = (queue.enqueueOne(start) *>
  //     // TODO: Don't hardcode the number of steps to let through.
  //     (handler(queue).take(4).run.exec(qs1))).unsafePerformSync
  //   assert(result.queue.pending.length == 2)
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
