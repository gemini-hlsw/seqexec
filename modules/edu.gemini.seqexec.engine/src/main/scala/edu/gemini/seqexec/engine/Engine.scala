// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import scalaz._

import edu.gemini.seqexec.model.Model.{Conditions, Operator}

/**
  * A Map of `Sequence`s.
  */
final case class Engine[+A](sequences: Map[Sequence.Id, Sequence[A]])

object Engine {

  type Id = String

  def sequences[A]: Engine[A] @> Map[Sequence.Id, Sequence[A]] =
    Lens.lensu((q, s) => q.copy(sequences = s), _.sequences)

  def empty[A]: Engine[A] = Engine(Map.empty)

// This fails to compile with the error "not found: type $anon"
//  implicit val engineFunctor = new Functor[Engine] {
//    def map[A, B](q: Engine[A])(f: A => B): Engine[B] =
//      Engine(q.sequences.mapValues(_.map(f)))
//  }

  final case class State(conditions: Conditions, operator: Option[Operator], sequences: Map[Sequence.Id, Sequence.State])

  object State {

    def empty: State = State(Conditions.default, None, Map.empty)

  }

}
