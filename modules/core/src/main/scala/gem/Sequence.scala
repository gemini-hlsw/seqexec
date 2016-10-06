package gem

import scalaz._
import Scalaz._

object Sequence {

  trait Id {
    def oid: Observation.Id
    def name: String
  }

  object Id extends ((Observation.Id, String) => Option[Sequence.Id]) {
    def apply(o: Observation.Id, n: String): Option[Sequence.Id] =
      !(n.contains('-') || "" === n.trim) option new Sequence.Id {
        override val oid: Observation.Id = o
        override val name: String        = n.trim

        override def toString: String =
          s"${oid.toString}-$n"
      }

    def fromString(s: String): Option[Sequence.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          Observation.Id.fromString(a).flatMap { Id(_, b.drop(1)) }
      }

    def unsafeFromString(s: String): Sequence.Id =
      fromString(s) | sys.error(s"Malformed Sequence.Id: $s")

    def unapply(arg: Id): Option[(Observation.Id, String)] =
      Some((arg.oid, arg.name))
  }

}
