// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats._
import cats.implicits._

import gem.enum.Guider

/**
 * Associates one or more guide probes or guide windows with guide stars.
 */
sealed trait GuideGroup {

  def toMap: Map[Guider, Target]

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
   * Guide "group" for the majority of cases where there is a single guide star.
   *
   * @param guider guide probe or guide window
   * @param target guide star tracked by the guider
   */
  sealed abstract class SingleGuiderGroup(val guider: Guider, target: Target) extends GuideGroup with Product with Serializable {

    override val toMap: Map[Guider, Target] =
      Map(guider -> target)

  }

  object Flamingos2 {
    final case class OI(target: Target) extends SingleGuiderGroup(Guider.F2OI, target)
    final case class P1(target: Target) extends SingleGuiderGroup(Guider.P1GS, target)
    final case class P2(target: Target) extends SingleGuiderGroup(Guider.P2GS, target)
  }

  object GmosNorth {
    final case class OI(target: Target) extends SingleGuiderGroup(Guider.GmosNOI, target)
    final case class P1(target: Target) extends SingleGuiderGroup(Guider.P1GN,    target)
    final case class P2(target: Target) extends SingleGuiderGroup(Guider.P2GN,    target)
  }

  object GmosSouth {
    final case class OI(target: Target) extends SingleGuiderGroup(Guider.GmosSOI, target)
    final case class P1(target: Target) extends SingleGuiderGroup(Guider.P1GS,    target)
    final case class P2(target: Target) extends SingleGuiderGroup(Guider.P2GS,    target)
  }

}

//sealed trait GmosSouthGuideGroup
//
//object GmosSouthGuideGroup {
//  final case class Oi(star: Target) extends GmosSouthGuideGroup
//  final case class P1(star: Target) extends GmosSouthGuideGroup
//  final case class P2(star: Target) extends GmosSouthGuideGroup
//}
//
//sealed trait OiGuideGroupType
//
//object OiGuideGroupType {
//  case object Oi extends OiGuideGroupType
//  case object P1 extends OiGuideGroupType
//  case object P2 extends OiGuideGroupType
//}
//
//final case class OiGuideGroup(s: Target, t: OiGuideGroupType)
//
//sealed trait GsaoiGuideGroup
//
//object GsaoiGuideGroup {
//  final case class Gems(cwfs1: Option[Target], cwfs2: Option[Target], cwfs3: Target) extends GsaoiGuideGroup
//  final case class P1(star: Target)                                                  extends GsaoiGuideGroup
//}
//
//sealed abstract class GuideEnvironment[G](auto: Option[G], manual: Either[List[G], Zipper[G]]) {
//
//  def selected: Option[G] =
//    manual.fold(_ => auto, Some(_.focus))
//
//}
//
//final case class GmosSouthGuideEnvironment(e: GuideEnvironment[OiGuideGroup])
