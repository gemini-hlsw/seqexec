// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats.Eq
import cats.data.NonEmptyList
import gem.enum.Instrument
import monocle.macros.Lenses


sealed trait Asterism extends Product with Serializable {

  type I <: Instrument with Singleton

  def instrument: Instrument

  def targets: NonEmptyList[Target]

}

object Asterism {

  type Aux[I0] = Asterism { type I = I0 }

  sealed abstract class Impl[I0 <: Instrument with Singleton](val instrument: I0) extends Asterism {
    type I = I0
  }

  @Lenses final case class SingleTarget[I0 <: Instrument with Singleton](target: Target, override val instrument: I0) extends Asterism.Impl(instrument) {

    override def targets: NonEmptyList[Target] =
      NonEmptyList.one(target)

  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object SingleTarget {
    implicit def EqSingleTarget[I <: Instrument with Singleton]: Eq[SingleTarget[I]] =
      Eq.fromUniversalEquals
  }

  @Lenses final case class GhostDualTarget(ifu1: Target, ifu2: Target) extends Asterism.Impl(Instrument.Ghost) {

    override def targets: NonEmptyList[Target] =
      NonEmptyList.of(ifu1, ifu2)
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object GhostDualTarget {
    implicit val EqGhostDualTarget: Eq[GhostDualTarget] =
      Eq.fromUniversalEquals
  }

  implicit val EqAsterism: Eq[Asterism] =
    Eq.fromUniversalEquals

}