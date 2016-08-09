package edu.gemini.seqexec.engine

import scala.collection.immutable.IntMap

import Engine._
import Event._
import Handler._
import State._
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
    _ <- Task.delay { println("System: Start TCS configuration") }
    _ <- Task.delay { Thread.sleep(2000) }
    _ <- Task.delay { println ("System: Complete TCS configuration") }
  } yield OK

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  val configureInst: Action  = for {
    _ <- Task.delay { println("System: Start Instrument configuration") }
    _ <- Task.delay { Thread.sleep(2000) }
    _ <- Task.delay { println("System: Complete Instrument configuration") }
  } yield OK

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = for {
    _ <- Task.delay { println("System: Start observation") }
    _ <- Task.delay { Thread.sleep(2000) }
    _ <- Task.delay { println ("System: Complete observation") }
  } yield OK

  val faulty: Action  = for {
    _ <- Task.delay { println("System: Start observation") }
    _ <- Task.delay { Thread.sleep(1000) }
    _ <- Task.delay { println ("System: Complete observation") }
  } yield OK

  val seqstatus1: SeqStatus = SeqStatus(
    Sequence(
      List(),
      IntMap(),
      List(
        List(configureTcs, configureInst),
        List(observe),
        List(configureTcs, configureInst),
        List(observe)
      )),
    Waiting)

  val emptySeqStatus: SeqStatus =
    SeqStatus(Sequence(List(), IntMap(), List()), Waiting)

  val queue = async.boundedQueue[Event](10)

  def request(ev: Event): Task[SeqStatus] =
    queue.enqueueOne(start) *>
      (handler(queue).take(1).run.exec(emptySeqStatus))

  it should "be in running Status after a Start event" in {
    val response = status.get(request(start).unsafePerformSync)
    assert(response === Running)
  }

  it should "0 steps pending after 4 steps have been processed in seqstatus1" in {
    val result = queue.enqueueOne(start) *>
      (handler(queue).take(4).run.exec(seqstatus1))
    // TODO: `pending` clashes with something brought in scope
    // which I don't know how to hide
    assert(sequence.andThen(State.pending).get(result.unsafePerformSync).length == 0)
  }

  it should "finish raising a Terminated exception after an Exit event" in {
    intercept[Cause.Terminated](
      (queue.enqueueOne(exit) *>
         (handler(queue).run.exec(emptySeqStatus))
      ).unsafePerformSync
    )
  }
}
