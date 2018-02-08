// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.{ Applicative, Eval, Traverse }
import cats.implicits._

import gem.syntax.treemap._

import scala.collection.immutable.TreeMap

/**
 * A science program, the root data type in the science model, parameterized over the type of its
 * observations, typically `[[gem.Observation Observation[α,β]]]` for a fully-specified program,
 * or `Nothing` for a minimally-specified program.
 * @group Program Model
 */
final case class Program[+A](id: Program.Id, title: String, observations: TreeMap[Observation.Index, A])

object Program {

  type Id                 = ProgramId
  val  Id: ProgramId.type = ProgramId

  /** Program is a traversable functor. */
  implicit val ProgramTraverse: Traverse[Program] =
    new Traverse[Program] {
      def foldLeft[A, B](fa: Program[A], b: B)(f: (B, A) => B): B =
        fa.observations.values.toList.foldLeft(b)(f)
      def foldRight[A, B](fa: Program[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.observations.values.toList.foldRight(lb)(f)
      def traverse[G[_]: Applicative, A, B](fa: Program[A])(f: A => G[B]): G[Program[B]] =
        fa.observations.values.toList.traverse(f).map { bs =>
          fa.copy(observations = TreeMap.fromList(fa.observations.keys.toList.zip(bs)))
        }
    }

}
