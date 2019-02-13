// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import cats._
import cats.implicits._

sealed trait GuideEnvironment extends Product with Serializable

object GuideEnvironment {

  final case class Flamingos2(options: GuideOptions[GuideGroup.Flamingos2]) extends GuideEnvironment
  final case class GmosNorth(options: GuideOptions[GuideGroup.GmosNorth ])  extends GuideEnvironment
  final case class GmosSouth(options: GuideOptions[GuideGroup.GmosSouth ])  extends GuideEnvironment

  // placeholder until the other cases are defined
  case object NoGuiding extends GuideEnvironment

  object Flamingos2 {
    val empty: Flamingos2 =
      Flamingos2(GuideOptions.empty[GuideGroup.Flamingos2])

    def typed(o: Option[GuideEnvironment]): Flamingos2 =
      o match {
        case Some(e @ Flamingos2(_)) => e
        case _                       => empty
      }

    implicit val EqFlamingos2: Eq[Flamingos2] =
      Eq.instance { (a, b) => a.options  === b.options }
  }

  object GmosNorth {
    val empty: GmosNorth =
      GmosNorth(GuideOptions.empty[GuideGroup.GmosNorth])

    def typed(o: Option[GuideEnvironment]): GmosNorth =
      o match {
        case Some(e @ GmosNorth(_)) => e
        case _                      => empty
      }

    implicit val EqGmosNorth: Eq[GmosNorth] =
      Eq.instance { (a, b) => a.options  === b.options }
  }

  object GmosSouth {
    val empty: GmosSouth =
      GmosSouth(GuideOptions.empty[GuideGroup.GmosSouth])

    def typed(o: Option[GuideEnvironment]): GmosSouth =
      o match {
        case Some(e @ GmosSouth(_)) => e
        case _                      => empty
      }

    implicit val EqGmosSouth: Eq[GmosSouth] =
      Eq.instance { (a, b) => a.options  === b.options }
  }

  implicit val EqGuideEnvironment: Eq[GuideEnvironment] =
    Eq.instance {
      case (a: Flamingos2, b: Flamingos2) => a === b
      case (a: GmosNorth,  b: GmosNorth ) => a === b
      case (a: GmosSouth,  b: GmosSouth ) => a === b
      case (NoGuiding,     NoGuiding    ) => true
      case _                              => false
    }

}