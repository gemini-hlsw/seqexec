package edu.gemini.seqexec.engine

import Event._
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.Process

object Handler {

  /**
    * Main logical thread to handle events and produce output.
    */
  private def handler(q: EventQueue)(ev: Event): Engine[QState] = {

    def handleUserEvent(ue: UserEvent): Engine[Unit] = ue match {
      case Start              =>
        log("Output: Started") *> switch(q)(Status.Running)
      case Pause              =>
        log("Output: Paused") *> switch(q)(Status.Waiting)
      case Poll               =>
        log("Output: Polling current state")
      case Exit               =>
        log("Bye") *> close(q)
    }

    def handleSystemEvent(se: SystemEvent): Engine[Unit] = se match {
      case (Completed(i, r)) =>
        log("Output: Action completed") *> complete(i, r)
      case (Failed(i, e))    =>
        log("Output: Action failed") *> fail(q)(i, e)
      case Executed          =>
        log("Output: Execution completed, launching next execution") *> next(q)
      // TODO: Closing to facilitate testing, in reality it shouldn't close
      case Finished          =>
        log("Output: Finished") *> switch(q)(Status.Completed) *> close(q)
    }

    (ev match {
        case EventUser(ue)   => handleUserEvent(ue)
        case EventSystem(se) => handleSystemEvent(se)
      }) *> get
  }

  // Kudos to @tpolecat
  /** Traverse a process with a stateful computation. */
  def mapEvalState[F[_]: Monad: Catchable, A, S, B](
    fs: Process[F, A], s: S, f: A => StateT[F, S, B]
  ): Process[F, B] = {
    def go(fs: Process[F, A], s: S): Process[F, B] =
      Process.eval(fs.unconsOption).flatMap {
        case None         => Process.halt
        case Some((h, t)) => Process.eval(f(h).run(s)).flatMap {
          case (s, a) => Process.emit(a) ++ go(t, s)
        }
      }
    go(fs, s)
  }

  private def handlerE(q: EventQueue)(ev: Event): Engine[(Event, QState)] =
    handler(q)(ev).map((ev, _))

  def processT(q: EventQueue)(qs: QState): Process[Task, (Event, QState)] =
    mapEvalState(q.dequeue, qs, handlerE(q))

  def processE(q: EventQueue): Process[Engine, (Event, QState)] =
    receive(q).evalMap(handlerE(q))
}
