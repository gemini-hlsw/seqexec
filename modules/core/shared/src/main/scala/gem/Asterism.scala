// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import cats.data.NonEmptyList
import monocle.macros.Lenses


sealed abstract class Asterism extends Product with Serializable {
  def targets: NonEmptyList[Target]
}

object Asterism {

  @Lenses
  final case class SingleTarget(target: Target) extends Asterism {
    override def targets: NonEmptyList[Target] =
      NonEmptyList.one(target)
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object SingleTarget {
    implicit def EqSingleTarget: Eq[SingleTarget] =
      Eq.fromUniversalEquals
  }

  @Lenses
  final case class DualTarget(ifu1: Target, ifu2: Target) extends Asterism {
    override def targets: NonEmptyList[Target] =
      NonEmptyList.of(ifu1, ifu2)
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object DualTarget {
    implicit val EqDualTarget: Eq[DualTarget] =
      Eq.fromUniversalEquals
  }

  implicit def EqAsterism: Eq[Asterism] =
    Eq.fromUniversalEquals

}