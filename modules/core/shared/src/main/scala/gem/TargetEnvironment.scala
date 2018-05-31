// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import cats.implicits._
import scala.collection.immutable.TreeSet

/** Collection of targets associated with an observation. */
sealed trait TargetEnvironment {
  def userTargets: TreeSet[UserTarget]
}
sealed trait SingleTargetEnvironment extends TargetEnvironment {
  def scienceTarget: Option[Target]
}

object TargetEnvironment {

  final case class DefaultSingleTarget(
    scienceTarget: Option[Target],
    userTargets:   TreeSet[UserTarget]
  ) extends SingleTargetEnvironment
  object DefaultSingleTarget {
    def empty: DefaultSingleTarget =
      DefaultSingleTarget(None, TreeSet.empty)
  }

  final case class GhostDualTarget(
    scienceTarget: Option[GhostDualTarget.ScienceTarget],
    userTargets:   TreeSet[UserTarget]
  ) extends TargetEnvironment
  object GhostDualTarget {

    final case class ScienceTarget(ifu1: Target, ifu2: Target)

    def empty: GhostDualTarget =
      GhostDualTarget(None, TreeSet.empty)

  }

  implicit def EqTargetEnvironment: Eq[TargetEnvironment] =
    Eq.fromUniversalEquals

}