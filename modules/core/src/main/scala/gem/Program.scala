// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import scalaz._, Scalaz._

/**
 * A science program, the root data type in the science model, parameterized over the type of its
 * observations, typically `[[gem.Observation Observation[α,β]]]` for a fully-specified program,
 * or `Nothing` for a minimally-specified program.
 * @group Program Model
 */
final case class Program[A](id: Program.Id, title: String, observations: List[A])

object Program {

  type Id                 = ProgramId
  val  Id: ProgramId.type = ProgramId

  /** Program is a traversable functor. */
  implicit val ProgramTraverse: Traverse[Program] =
    new Traverse[Program] {
      def traverseImpl[G[_]: Applicative, A, B](fa: Program[A])(f: A => G[B]): G[Program[B]] =
        fa.observations.traverse(f).map(os => fa.copy(observations = os))
    }

}
