package edu.gemini.seqexec.engine

import org.scalatest.FlatSpec
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process
import scalaz.stream.async
import scalaz.stream.async.mutable.Queue

import edu.gemini.seqexec.engine.Engine._
import edu.gemini.seqexec.engine.Handler._
import edu.gemini.seqexec.engine.Sequence._

class HandlerSpec extends FlatSpec {

  /**
    * Emulates TCS configuration in the real world.
    *
    */
  val configureTcs: Action  = for {
    _ <- Task.delay { println("System: Start TCS configuration") }
    _ <- Task.delay { Thread.sleep(2000) }
    _ <- Task.delay { println ("System: Complete TCS configuration") }
  } yield Done

  /**
    * Emulates Instrument configuration in the real world.
    *
    */
  def configureInst: Action  = for {
    _ <- Task.delay { println("System: Start Instrument configuration") }
    _ <- Task.delay { Thread.sleep(2000) }
    _ <- Task.delay { println("System: Complete Instrument configuration") }
  } yield Done

  /**
    * Emulates an observation in the real world.
    *
    */
  val observe: Action  = for {
    _ <- Task.delay { println("System: Start observation") }
    _ <- Task.delay { Thread.sleep(2000) }
    _ <- Task.delay { println ("System: Complete observation") }
  } yield Done

  val faulty: Action  = for {
    _ <- Task.delay { println("System: Start observation") }
    _ <- Task.delay { Thread.sleep(1000) }
    _ <- Task.delay { println ("System: Complete observation") }
  } yield Error

  val sequence0: Sequence =
    List(
      List(configureTcs, configureInst),
      List(observe),
      List(configureTcs, configureInst),
      List(observe)
    )

  val queue = async.boundedQueue[Event](10)

  def tester(queue: Queue[Event]): Task[Unit] =
    Task.delay {
      Thread.sleep(100)
      queue.enqueueOne(start)
      Thread.sleep(2000)
      queue.enqueueOne(pause)
      Thread.sleep(3000)
      // Add a failing step
      queue.enqueueOne(addStep(List(faulty, observe)))
      queue.enqueueOne(start)
    }

  // XXX: This doesn't typecheck :(
  // Nondeterminism[Task].both(
  //   tester(queue),
  //   handler(queue).run.exec((sequence0, Waiting))
  // ).unsafePerformSync
}
