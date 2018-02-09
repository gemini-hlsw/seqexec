// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import cats.implicits._
import gem.enum.Instrument
import scala.collection.immutable.TreeSet

/** Collection of targets associated with an observation. */
sealed trait TargetEnvironment {

  type I <: Instrument with Singleton

  val asterism: Option[Asterism.Aux[I]]

  /** Guide stars. */
  val userTargets: TreeSet[UserTarget]

  override final def toString =
    s"TargetEnvironment($asterism, $userTargets)"

}

object TargetEnvironment {

  // Because of the existental buried in the option we need two sets of constructors, one set
  // that preserves the Asterism's refinement when present (Aux.apply/empty) and another that
  // doesn't. It may be possible to improve this but I tried for a few hours and this is the best
  // I can do.

  def apply(a: Option[Asterism], ts: TreeSet[UserTarget]): TargetEnvironment =
    a match { // yes, in order to get precise types we need this pattern-match
      case None    => Aux(None, ts)
      case Some(a) => Aux(Some(a: Asterism.Aux[a.I]), ts)
    }

  def empty: TargetEnvironment =
    apply(None, TreeSet.empty)

  type Aux[I0 <: Instrument with Singleton] =
    TargetEnvironment { type I = I0 }

  object Aux {

    // Implement with a case class for nice equals, etc.
    private final case class Impl[I0 <: Instrument with Singleton](
      asterism:    Option[Asterism.Aux[I0]],
      userTargets: TreeSet[UserTarget]
    ) extends TargetEnvironment {
      type I = I0
    }

    def apply[I0 <: Instrument with Singleton](
      a:  Option[Asterism.Aux[I0]],
      ts: TreeSet[UserTarget]
    ): TargetEnvironment.Aux[I0] =
      Impl(a, ts)

    def empty[I <: Instrument with Singleton]: TargetEnvironment.Aux[I] =
      apply(None, TreeSet.empty)

  }

  implicit def EqTargetEnvironment[I <: Instrument with Singleton]: Eq[TargetEnvironment.Aux[I]] =
    Eq.fromUniversalEquals

}