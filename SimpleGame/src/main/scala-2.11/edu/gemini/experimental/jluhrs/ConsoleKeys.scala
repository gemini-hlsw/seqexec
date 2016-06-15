package edu.gemini.experimental.jluhrs

import edu.gemini.experimental.jluhrs.EventStream._
import edu.gemini.experimental.jluhrs.Game._

import scalaz.concurrent.Task
import scalaz.stream.{Process, async}
import scala.concurrent.duration._
import sys.process._

/**
  * Created by jluhrs on 6/15/16.
  */
object ConsoleKeys {
  // Build a process that produces player inputs from keystrokes
  private val inputQ = async.boundedQueue[Event[PlayerInput]](10)

  val inputP: Process[Task, Event[PlayerInput]] = inputQ.dequeue

  private val keySource: Task[Unit] = Task {

    // Some munbo jumbo to get keystrokes unbuffered.
    (Seq("sh", "-c", "stty -icanon min 1 < /dev/tty") !)
    (Seq("sh", "-c", "stty -echo < /dev/tty") !)

    while (true) {
      val c = Console.in.read
      c match {
        case 119 => inputQ.enqueueOne((System.nanoTime nanoseconds, Up)).unsafePerformSync
        case  97 => inputQ.enqueueOne((System.nanoTime nanoseconds, Left)).unsafePerformSync
        case 115 => inputQ.enqueueOne((System.nanoTime nanoseconds, Down)).unsafePerformSync
        case 100 => inputQ.enqueueOne((System.nanoTime nanoseconds, Right)).unsafePerformSync
        case _ => ()
      }
    }
    ()
  }

  def start = keySource.unsafePerformAsync(_ => ())

}
