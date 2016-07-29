package gem

import scalaz._, Scalaz._

case class Observation(id: Observation.Id, title: String, instrument: Option[String])

object Observation {
  case class Id(pid: Program.Id, index: Int) {
    override def toString = s"$pid-$index"
  }
  object Id {

    def fromString(s: String): Option[Observation.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          b.drop(1).parseInt.toOption.map { n =>
            Observation.Id(Program.Id.parse(a), n)
          }
      }

    def unsafeFromString(s: String): Observation.Id =
      fromString(s).getOrElse(sys.error("Malformed Observation.Id: " + s))

  }
}