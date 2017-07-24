package engine

sealed trait Event
object Event {
  case object Start extends Event
  case object Stop extends Event
}
