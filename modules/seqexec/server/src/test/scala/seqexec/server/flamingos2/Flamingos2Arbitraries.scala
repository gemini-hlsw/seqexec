// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.flamingos2

import edu.gemini.spModel.gemini.flamingos2.{Flamingos2 => LegacyF2}
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait Flamingos2Arbitraries {
  implicit val f2FPUArb: Arbitrary[LegacyF2.FPUnit] = Arbitrary(Gen.oneOf(LegacyF2.FPUnit.values()))
  implicit val f2FPUCogen: Cogen[LegacyF2.FPUnit] =
    Cogen[String].contramap(_.displayValue())
  implicit val f2CFPUArb: Arbitrary[Flamingos2Controller.FocalPlaneUnit] = Arbitrary(Gen.oneOf(Flamingos2Controller.FocalPlaneUnit.Open, Flamingos2Controller.FocalPlaneUnit.GridSub1Pix,
    Flamingos2Controller.FocalPlaneUnit.Grid2Pix, Flamingos2Controller.FocalPlaneUnit.Slit1Pix, Flamingos2Controller.FocalPlaneUnit.Slit2Pix,
    Flamingos2Controller.FocalPlaneUnit.Slit3Pix, Flamingos2Controller.FocalPlaneUnit.Slit4Pix, Flamingos2Controller.FocalPlaneUnit.Slit6Pix, Flamingos2Controller.FocalPlaneUnit.Slit8Pix))
  implicit val f2CFPUCogen: Cogen[Flamingos2Controller.FocalPlaneUnit] =
    Cogen[String].contramap(_.productPrefix)
}
