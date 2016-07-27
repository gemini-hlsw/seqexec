package gem

case class Observation(id: Observation.Id, title: String, instrument: Option[String])

object Observation {
  case class Id(pid: Program.Id, index: Int) {
    override def toString = s"$pid-$index"
  }
}