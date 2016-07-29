package edu.gemini.seqexec.engine

import scala.concurrent.Channel

import scalaz._
import Scalaz._
import edu.gemini.seqexec.engine.Engine._

object Handler {

  /**
    * Main logical thread to handle events and produce output.
    */
  def handler[R](chan: Channel[Event]): Telescope[R] = {

    def handleUserEvent(ue: UserEvent): Telescope[Unit] = ue match {
      case Start => log("Output: Started") *> switch(Running) *> run(chan)
      case Pause => log("Output: Paused") *> switch(Waiting)
      case AddStep(ste) => log("Output: Adding Step") *> add(ste)
    }

    def handleSystemEvent(se: SystemEvent) = se match {
      case Completed => log("Output: Action completed")
      case Failed => log("Output: Action failed")
      case Synced => log("Output: Parallel actions completed") *> tail *> run(chan)
      case SyncFailed => log("Output: Step failed. Repeating...") *> run(chan)
      case Finished => log("Output: Finished") *> switch(Waiting)
    }

    (receive(chan) >>= {
       (ev: Event) => ev match {
         case EventUser(ue) => handleUserEvent(ue)
         case EventSystem(se) => handleSystemEvent(se)
       }
     }
    // XXX: `forever`, for some reason, keeps looping over the same message even
    // if only sent once.
    ) >> handler(chan)
  }
}
