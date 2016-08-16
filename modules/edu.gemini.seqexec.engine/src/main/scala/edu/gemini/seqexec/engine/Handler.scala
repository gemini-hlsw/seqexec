package edu.gemini.seqexec.engine

import Event._
import State._
import Engine._
import scalaz.Scalaz._
import scalaz.stream.Process
import scalaz.stream.async.mutable.Queue

object Handler {

  /**
    * Main logical thread to handle events and produce output.
    */
  def handler(queue: Queue[Event]): Process[Telescope, SeqStatus] = {

    def handleUserEvent(ue: UserEvent): Telescope[SeqStatus] = ue match {
      case Start => log("Output: Started") *> switch(queue)(Running)
      case Pause => log("Output: Paused") *> switch(queue)(Waiting)
      case AddStep(ste) => log("Output: Adding Step") *> add(ste)
      case Poll => log("Output: Poll current state") *> ask
      case Exit => log("Bye") *> close(queue)
    }

    def handleSystemEvent(se: SystemEvent): Telescope[SeqStatus] = se match {
      case (Completed(i)) => log("Output: Action completed") *> complete(queue)(i)
      case (Failed(i)) => log("Output: Action failed") *> fail(queue)(i)
      case Finished => log("Output: Finished")
    }

    receive(queue) >>= (
      ev => Process.eval (
        ev match {
          case EventUser(ue) => handleUserEvent(ue)
          case EventSystem(se) => handleSystemEvent(se)
        }
      )
    )
  }
}
