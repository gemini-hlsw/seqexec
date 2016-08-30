package edu.gemini.seqexec.engine

object Event {
  /**
    * Anything that can go through the Event Queue.
    *
    */
  sealed trait Event
  case class EventUser(ue: UserEvent) extends Event
  case class EventSystem(se: SystemEvent) extends Event

  sealed trait UserEvent
  case object Start extends UserEvent
  case object Pause extends UserEvent
  case object Poll extends UserEvent
  case class AddExecution(pend: Execution.Pending) extends UserEvent
  case object Exit extends UserEvent

  val start: Event = EventUser(Start)
  val pause: Event = EventUser(Pause)
  val poll: Event = EventUser(Poll)
  def addExecution(pend: Execution.Pending): Event = EventUser(AddExecution(pend))
  val exit: Event = EventUser(Exit)

  sealed trait SystemEvent
  case class Completed(i: Int) extends SystemEvent
  case class Failed(i: Int) extends SystemEvent
  case object Executed extends SystemEvent
  case object Finished extends SystemEvent

  def completed(i: Int): Event = EventSystem(Completed(i))
  def failed(i: Int): Event = EventSystem(Failed(i))
  val executed: Event = EventSystem(Executed)
  val finished: Event = EventSystem(Finished)
}
