package edu.gemini.seqexec.engine

import Sequence._

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
  case class AddStep(ste: Step) extends UserEvent
  case object Exit extends UserEvent

  val start: Event = EventUser(Start)
  val pause: Event = EventUser(Pause)
  def addStep(ste: Step): Event = EventUser(AddStep(ste))
  val exit: Event = EventUser(Exit)

  sealed trait SystemEvent
  // when an action is completed even if it belongs to a set of
  // parallel actions.
  case object Completed extends SystemEvent
  // when an action failed.
  case object Failed extends SystemEvent
  // when a set of parallel actions is completed.
  case object Synced extends SystemEvent
  case object SyncFailed extends SystemEvent
  case object Finished extends SystemEvent

  val completed: Event = EventSystem(Completed)
  val failed: Event = EventSystem(Failed)
  val synced: Event = EventSystem(Synced)
  val syncFailed: Event = EventSystem(SyncFailed)
  val finished: Event = EventSystem(Finished)
}
