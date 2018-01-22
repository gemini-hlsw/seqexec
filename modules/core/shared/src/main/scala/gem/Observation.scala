// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.syntax.string._

import mouse.boolean._

import cats.{ Applicative, Eval, Order, Show, Bitraverse }
import cats.implicits._

/**
 * An observation, parameterized over the types of its static config and steps (typically
 * [[gem.config.StaticConfig StaticConfig]] and [[gem.Step Step[α]]], respectively, for a
 * fully-specified Observation; or `Unit` and `Nothing` for a
 * minimally-specified Observation.
 * @group Program Model
 */
final case class Observation[+S, +D](
  title: String,
  targets: TargetEnvironment,
  staticConfig: S,
  steps: List[D])

object Observation {

  /** A positive, non-zero integer for use in ids. */
  sealed abstract case class Index(toInt: Int) {
    def format: String =
      s"$toInt"
  }

  object Index {
    def fromInt(i: Int): Option[Index] =
      (i > 0) option new Index(i) {}

    def unsafeFromInt(i: Int): Index =
      fromInt(i).getOrElse(sys.error(s"Negative index: $i"))

    def fromString(s: String): Option[Index] =
      s.parseIntOption.filter(_ > 0).map(new Index(_) {})

    def unsafeFromString(s: String): Index =
      fromString(s).getOrElse(sys.error(s"Malformed observation index: '$s'"))

    implicit val OrderIndex: Order[Index] =
      Order.by(_.toInt)

    implicit val OrderingIndex: scala.math.Ordering[Index] =
      OrderIndex.toOrdering

    implicit val showIndex: Show[Index] =
      Show.fromToString
  }

  /** An observation is identified by its program and a serial index. */
  final case class Id(pid: Program.Id, index: Index) {
    def format: String =
      s"${pid.format}-${index.format}"
  }
  object Id {

    def fromString(s: String): Option[Observation.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          Index.fromString(b.drop(1)).flatMap { i =>
            Program.Id.fromString(a).map(Observation.Id(_, i))
          }
      }

    def unsafeFromString(s: String): Observation.Id =
      fromString(s).getOrElse(sys.error("Malformed Observation.Id: " + s))

    /** Observations are ordered by program id and index. */
    implicit val OrderId: Order[Id] =
      Order.by(a => (a.pid, a.index))

    implicit val OrderingId: scala.math.Ordering[Id] =
      OrderId.toOrdering

    implicit val showId: Show[Id] =
      Show.fromToString

  }

  /** Observation is a bitraversable functor. */
  implicit val ObservationBitraverse: Bitraverse[Observation] =
    new Bitraverse[Observation] {
      def bifoldLeft[A, B, C](fab: Observation[A,B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
        fab.steps.foldLeft(f(c, fab.staticConfig))(g)
      def bifoldRight[A, B, C](fab: Observation[A,B], c: Eval[C])(f: (A, Eval[C]) => Eval[C], g: (B, Eval[C]) => Eval[C]): Eval[C] =
        fab.steps.foldRight(f(fab.staticConfig, c))(g)
      def bitraverse[G[_]: Applicative, A, B, C, D](fab: Observation[A, B])(f: A => G[C], g: B => G[D]): G[Observation[C,D]] =
        (f(fab.staticConfig), fab.steps.traverse(g)).mapN((c, d) => fab.copy(staticConfig = c, steps = d))
    }

}
