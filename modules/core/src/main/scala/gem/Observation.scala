package gem

import scalaz._, Scalaz._

case class Observation[S, D](
  id: Observation.Id,
  title: String,
  staticConfig: S,
  steps: List[D])

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

    implicit val OrderingId: scala.math.Ordering[Id] =
      new scala.math.Ordering[Id] {
        def compare(x: Id, y: Id): Int =
          Program.OrderingProgramId.compare(x.pid, y.pid) match {
            case 0 => x.index compareTo y.index
            case i => i
          }
      }

    implicit val OrderId: Order[Id] =
      Order.fromScalaOrdering[Id]
  }

  implicit val ObservationBitraverse: Bitraverse[Observation] =
    new Bitraverse[Observation] {
      def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: Observation[A,B])(f: A => G[C], g: B => G[D]): G[Observation[C,D]] =
        (f(fab.staticConfig) |@| fab.steps.traverse(g))((c, d) => fab.copy(staticConfig = c, steps = d))
    }

}
