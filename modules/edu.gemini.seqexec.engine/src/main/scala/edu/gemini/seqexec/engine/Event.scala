package edu.gemini.seqexec.engine

object Event {
  /**
    * Anything that can go through the Event Queue.
    */
  sealed trait Event
  case class EventUser(ue: UserEvent) extends Event
  case class EventSystem(se: SystemEvent) extends Event

  /**
    * Events generated by the user.
    */
  sealed trait UserEvent
  case object Start extends UserEvent
  case object Pause extends UserEvent
  case object Poll extends UserEvent
  case object Exit extends UserEvent

  val start: Event = EventUser(Start)
  val pause: Event = EventUser(Pause)
  val poll: Event = EventUser(Poll)
  val exit: Event = EventUser(Exit)

  /**
    * Events generated internally by the Engine.
    */
  sealed trait SystemEvent
  case class Completed[R](i: Int, r: R) extends SystemEvent
  case class Failed[E](i: Int, e: E) extends SystemEvent
  case object Executed extends SystemEvent
  case object Finished extends SystemEvent

  def completed[R](i: Int, r: R): Event = EventSystem(Completed(i, r))
  def failed[E](i: Int, e: E): Event = EventSystem(Failed(i, e))
  val executed: Event = EventSystem(Executed)
  val finished: Event = EventSystem(Finished)
}
