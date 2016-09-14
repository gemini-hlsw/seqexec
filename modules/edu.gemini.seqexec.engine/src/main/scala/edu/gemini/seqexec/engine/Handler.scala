package edu.gemini.seqexec.engine

import Event._
import scalaz.Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

object Handler {

  /**
    * Main logical thread to handle events and produce output.
    */
  def handler(q: EventQueue): Process[Engine, QState] = {

    def handleUserEvent(ue: UserEvent): Engine[QState] = ue match {
      case Start => log("Output: Started") *> switch(q)(Status.Running)
      case Pause => log("Output: Paused") *> switch(q)(Status.Waiting)
      case AddExecution(pend) => log("Output: Adding Pending Execution") // *> add(pend)
      case Poll => log("Output: Polling current state")
      case Exit => log("Bye") *> close(q)
    }

    def handleSystemEvent(se: SystemEvent): Engine[QState] = se match {
      case (Completed(i)) => log("Output: Action completed") *> complete(i)
      case (Failed(i)) => log("Output: Action failed") *> fail(q)(i)
      case Executed => log("Output: Execution completed, launching next execution") *> next(q)
      case Finished => log("Output: Finished") *> close(q)
    }

    receive(q) >>= (
      ev => Process.eval(
        ev match {
          case EventUser(ue) => handleUserEvent(ue)
          case EventSystem(se) => handleSystemEvent(se)
        }
      )
    )
  }

  def run(q: EventQueue)(qs: QState): Task[QState] =
    handler(q).takeWhile(!_.pending.isEmpty).run.exec(qs)
}
