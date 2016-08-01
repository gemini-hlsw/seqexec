package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import scalaz.stream.Process
import scalaz.stream.async.mutable.Queue
import edu.gemini.seqexec.engine.Engine._

object Handler {

  /**
    * Main logical thread to handle events and produce output.
    */
  def handler(queue: Queue[Event]): Process[Telescope, SeqStatus] = {

    def handleUserEvent(ue: UserEvent): Telescope[SeqStatus] = ue match {
      case Start => log("Output: Started") *> switch(Running) *> run(queue)
      case Pause => log("Output: Paused") *> switch(Waiting)
      case AddStep(ste) => log("Output: Adding Step") *> add(ste)
    }

    def handleSystemEvent(se: SystemEvent): Telescope[SeqStatus] = se match {
      case Completed => log("Output: Action completed")
      case Failed => log("Output: Action failed")
      case Synced => log("Output: Parallel actions completed") *> tail *> run(queue)
      case SyncFailed => log("Output: Step failed. Repeating...") *> run(queue)
      case Finished => log("Output: Finished") *> switch(Waiting)
    }

    receive(queue) >>= (
      (ev: Event) => ev match {
        case EventUser(ue) => Process.eval(handleUserEvent(ue))
        case EventSystem(se) => Process.eval(handleSystemEvent(se))
      }
    )
  }
}
