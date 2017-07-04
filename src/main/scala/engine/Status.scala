package engine

sealed trait Status
object Status {
  case object Running extends Status
  case object Waiting extends Status
  case object Finished extends Status
  case object Failed extends Status
}
