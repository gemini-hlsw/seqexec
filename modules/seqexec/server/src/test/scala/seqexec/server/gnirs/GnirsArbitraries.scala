// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gnirs

import edu.gemini.spModel.gemini.gnirs.GNIRSParams
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen

trait GnirsArbitraries {

  implicit val gnirsAmArb: Arbitrary[GNIRSParams.AcquisitionMirror] = Arbitrary(Gen.oneOf(GNIRSParams.AcquisitionMirror.values()))
  implicit val gnirsAmCogen: Cogen[GNIRSParams.AcquisitionMirror] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsWpArb: Arbitrary[GNIRSParams.WollastonPrism] = Arbitrary(Gen.oneOf(GNIRSParams.WollastonPrism.values()))
  implicit val gnirsWpCogen: Cogen[GNIRSParams.WollastonPrism] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsSwArb: Arbitrary[GNIRSParams.SlitWidth] = Arbitrary(Gen.oneOf(GNIRSParams.SlitWidth.values()))
  implicit val gnirsSwCogen: Cogen[GNIRSParams.SlitWidth] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsCdArb: Arbitrary[GNIRSParams.CrossDispersed] = Arbitrary(Gen.oneOf(GNIRSParams.CrossDispersed.values()))
  implicit val gnirsCdCogen: Cogen[GNIRSParams.CrossDispersed] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsDeArb: Arbitrary[GNIRSParams.Decker] = Arbitrary(Gen.oneOf(GNIRSParams.Decker.values()))
  implicit val gnirsDeCogen: Cogen[GNIRSParams.Decker] =
    Cogen[String].contramap(_.displayValue())
  implicit val gnirsCaArb: Arbitrary[GNIRSParams.Camera] = Arbitrary(Gen.oneOf(GNIRSParams.Camera.values()))
  implicit val gnirsCaCogen: Cogen[GNIRSParams.Camera] =
    Cogen[String].contramap(_.displayValue())
}
