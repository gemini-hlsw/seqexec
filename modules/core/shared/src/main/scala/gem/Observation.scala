// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Functor, Order, Show }
import cats.implicits._
import gem.config.{ StaticConfig, DynamicConfig }
import gem.enum.Instrument
import gem.math.Index

/**
 * An observation, parameterized over the types of its targets, static config
 * and steps (typically [[gem.TargetEnvironment]], [[gem.config.StaticConfig StaticConfig]]
 * and [[gem.Step Step[α]]], respectively, for a fully-specified Observation;
 * or `Unit`, `Unit` and `Nothing` for a minimally-specified Observation.
 *
 * @group Program Model
 */
final case class Observation[+T, +S, +D](
  title: String,
  targets: T,
  staticConfig: S,
  steps: List[D])

object Observation {

  /** A fully specified observation, with unknown instrument. */
  type Full = Full.Aux[I] forSome { type I <: Instrument with Singleton }
  object Full {
    /** A fully specified observation, with the specified instrument. */
    type Aux[I <: Instrument with Singleton] =
      Observation[TargetEnvironment.Aux[I], StaticConfig.Aux[I], Step[DynamicConfig.Aux[I]]]
  }

  // Some syntax for Observation.Full
  implicit class ObservationFullOps(o: Observation.Full) {

      private def narrowImpl[I <: Instrument with Singleton](
        o: Observation.Full.Aux[I]
      ): (Instrument.Aux[I], Observation.Full.Aux[I]) =
        (o.staticConfig.instrument, o)

      /**
       * This lets is inspect an Observation.Full and get back a dependent pair that we can
       * pattern-match to dispatch based on the instrument; i.e., if the first element is
       * `Gnirs` then the second must be an `Observation.Full.Aux[GNirs.type]`, and
       * most importantly Scala understands this.
       */
      def narrow: (Instrument.Aux[I], Observation.Full.Aux[I]) forSome { type I <: Instrument with Singleton } =
        narrowImpl(o)

  }

  /** An observation is identified by its program and a serial index. */
  final case class Id(pid: Program.Id, index: Index) {
    def format: String =
      s"${pid.format}-${Index.fromString.reverseGet(index)}"
  }
  object Id {

    def fromString(s: String): Option[Observation.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          Index.fromString.getOption(b.drop(1)).flatMap { i =>
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

  /** A functor over `Observation` on the `T`, or targets, type parameter.
    * Not implicit.
    */
  def targetsFunctor[S, D]: Functor[Observation[?, S, D]] =
    new Functor[Observation[?, S, D]] {
      def map[A, B](o: Observation[A, S, D])(f: A => B): Observation[B, S, D] =
        Observation(o.title, f(o.targets), o.staticConfig, o.steps)
    }

  /** A functor over `Observation` on the `S`, or static configuration, type
    * parameter. Not implicit.
    */
  def staticConfigFunctor[T, D]: Functor[Observation[T, ?, D]] =
    new Functor[Observation[T, ?, D]] {
      def map[A, B](o: Observation[T, A, D])(f: A => B): Observation[T, B, D] =
        Observation(o.title, o.targets, f(o.staticConfig), o.steps)
    }

  /** A functor over `Observation` on the `D`, or dynamic configuration, type
    * parameter. Not implicit.
    */
  def dynamicConfigFunctor[T, S]: Functor[Observation[T, S, ?]] =
    new Functor[Observation[T, S, ?]] {
      def map[A, B](o: Observation[T, S, A])(f: A => B): Observation[T, S, B] =
        Observation(o.title, o.targets, o.staticConfig, o.steps.map(f))
    }
}
