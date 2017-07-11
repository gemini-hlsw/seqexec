// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import scalaz._, Scalaz._

final case class Observation[S, D](
  id: Observation.Id,
  title: String,
  staticConfig: S,
  steps: List[D])

object Observation {

  final case class Id(pid: Program.Id, index: Int) {
    def format: String =
      s"${pid.format}-$index"
  }
  object Id {

    def fromString(s: String): Option[Observation.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          b.drop(1).parseInt.toOption.flatMap { n =>
            Program.Id.fromString(a).map(Observation.Id(_, n))
          }
      }

    def unsafeFromString(s: String): Observation.Id =
      fromString(s).getOrElse(sys.error("Malformed Observation.Id: " + s))

    implicit val OrderId: Order[Id] =
      Order[Program.Id].contramap[Id](_.pid)   |+|
      Order[Int]       .contramap[Id](_.index)

    implicit val OrderingId: scala.math.Ordering[Id] =
      OrderId.toScalaOrdering

  }

  implicit val ObservationBitraverse: Bitraverse[Observation] =
    new Bitraverse[Observation] {
      def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: Observation[A,B])(f: A => G[C], g: B => G[D]): G[Observation[C,D]] =
        (f(fab.staticConfig) |@| fab.steps.traverse(g))((c, d) => fab.copy(staticConfig = c, steps = d))
    }

}
