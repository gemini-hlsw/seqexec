package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

import edu.gemini.seqexec.model.Model.{Conditions, Operator}

/**
  * A Map of `Sequence`s.
  */
case class Engine[+A](sequences: Map[Sequence.Id, Sequence[A]])

object Engine {

  type Id = String

  def sequences[A]: Engine[A] @> Map[Sequence.Id, Sequence[A]] =
    Lens.lensu((q, s) => q.copy(sequences = s), _.sequences)

  def empty[A]: Engine[A] = Engine(Map.empty)

  implicit val engineFunctor = new Functor[Engine] {
    def map[A, B](q: Engine[A])(f: A => B): Engine[B] =
      Engine(q.sequences.mapValues(_.map(f)))
  }

  case class State(conditions: Conditions, operator: Option[Operator], sequences: Map[Sequence.Id, Sequence.State])

  object State {

    def empty: State = State(Conditions.default, None, Map.empty)

  }

}
