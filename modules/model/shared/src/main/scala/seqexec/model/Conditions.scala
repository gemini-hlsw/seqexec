// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import monocle.macros.Lenses
import seqexec.model.enum._

@Lenses
final case class Conditions(
  cc: CloudCover,
  iq: ImageQuality,
  sb: SkyBackground,
  wv: WaterVapor
)

object Conditions {

  val Unknown: Conditions =
    Conditions(
      CloudCover.Unknown,
      ImageQuality.Unknown,
      SkyBackground.Unknown,
      WaterVapor.Unknown
    )

  val Worst: Conditions =
    Conditions(
      CloudCover.Any,
      ImageQuality.Any,
      SkyBackground.Any,
      WaterVapor.Any
    )

  val Nominal: Conditions =
    Conditions(
      CloudCover.Percent50,
      ImageQuality.Percent70,
      SkyBackground.Percent50,
      WaterVapor.Any
    )

  val Best: Conditions =
    Conditions(
      // In the ODB model it's 20% but that value it's marked as obsolete
      // so I took the non-obsolete lowest value.
      CloudCover.Percent50,
      ImageQuality.Percent20,
      SkyBackground.Percent20,
      WaterVapor.Percent20
    )

  val Default: Conditions =
    Unknown // Taken from ODB

  implicit val equalConditions: Eq[Conditions] =
    Eq.by(x => (x.cc, x.iq, x.sb, x.wv))


}
