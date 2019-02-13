// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats._
import cats.implicits._
import cats.data.NonEmptyList
import gem.enum.Guider

/**
 * Associates one or more guide probes or guide windows with guide stars.
 */
sealed trait GuideGroup extends Product with Serializable {

  def guideStars: NonEmptyList[(Guider, Target)]

}

object GuideGroup {

  /** Group identifier. */
  final case class Id(toInt: Int) extends AnyVal

  object Id {
    /** Ids ordered by wrapped integer value. */
    implicit val IdOrder: Order[Id] =
      Order.by(_.toInt)
  }

  /**
   * Guide "group" supertype for the majority of cases where there is a single
   * guide star.
   */
  sealed trait SingleGuider extends GuideGroup {

    def guider: Guider
    def target: Target

    override val guideStars: NonEmptyList[(Guider, Target)] =
      NonEmptyList.one(guider -> target)

  }

  sealed trait Flamingos2 extends SingleGuider

  object Flamingos2 {

    final case class OI(target: Target) extends Flamingos2 {
      override val guider: Guider = Guider.F2OI
    }

    final case class P1(target: Target) extends Flamingos2 {
      override val guider: Guider = Guider.P1GS
    }

    final case class P2(target: Target) extends Flamingos2 {
      override val guider: Guider = Guider.P2GS
    }

    implicit val EqFlamingos2: Eq[Flamingos2] =
      Eq.instance {
        case (OI(a), OI(b)) => a === b
        case (P1(a), P1(b)) => a === b
        case (P2(a), P2(b)) => a === b
        case _              => false
      }
  }

  sealed trait GmosNorth extends SingleGuider

  object GmosNorth {

    final case class OI(target: Target) extends GmosNorth {
      override val guider: Guider = Guider.GmosNOI
    }

    final case class P1(target: Target) extends GmosNorth {
      override val guider: Guider = Guider.P1GS
    }

    final case class P2(target: Target) extends GmosNorth {
      override val guider: Guider = Guider.P2GS
    }

    implicit val EqGmosNorth: Eq[GmosNorth] =
      Eq.instance {
        case (OI(a), OI(b)) => a === b
        case (P1(a), P1(b)) => a === b
        case (P2(a), P2(b)) => a === b
        case _              => false
      }

  }

  sealed trait GmosSouth extends SingleGuider

  object GmosSouth {

    final case class OI(target: Target) extends GmosSouth {
      override val guider: Guider = Guider.GmosSOI
    }

    final case class P1(target: Target) extends GmosSouth {
      override val guider: Guider = Guider.P1GS
    }

    final case class P2(target: Target) extends GmosSouth {
      override val guider: Guider = Guider.P2GS
    }

    implicit val EqGmosSouth: Eq[GmosSouth] =
      Eq.instance {
        case (OI(a), OI(b)) => a === b
        case (P1(a), P1(b)) => a === b
        case (P2(a), P2(b)) => a === b
        case _              => false
      }


  }

}