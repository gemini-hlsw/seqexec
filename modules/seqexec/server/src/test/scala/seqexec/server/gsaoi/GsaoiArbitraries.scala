// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

import edu.gemini.spModel.gemini.gsaoi.Gsaoi.ReadMode
import edu.gemini.spModel.gemini.gsaoi.Gsaoi.Roi
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import shapeless.tag
import seqexec.server.gsaoi.GsaoiController._
import seqexec.model.arb.ArbTime._

trait GsaoiArbitraries {

  implicit val windowCoverArb: Arbitrary[WindowCover] =
    Arbitrary(Gen.oneOf(WindowCover.Closed, WindowCover.Opened))
  implicit val windowCoverCogen: Cogen[WindowCover] =
    Cogen[String].contramap(_.productPrefix)

  implicit val DCConfigArb: Arbitrary[DCConfig] =
    Arbitrary {
      for {
        readMode           <- arbitrary[ReadMode]
        roi                <- arbitrary[Roi]
        coadds             <- arbitrary[Int]
        exposureTime       <- arbitrary[ExposureTime]
        numberOfFowSamples <- arbitrary[Int]
      } yield
        DCConfig(readMode,
                 roi,
                 tag[CoaddsI][Int](coadds),
                 exposureTime,
                 tag[NumberOfFowSamplesI][Int](numberOfFowSamples))
    }

  implicit val readModeCogen: Cogen[ReadMode] =
    Cogen[String].contramap(_.displayValue())

  implicit val roiCogen: Cogen[Roi] =
    Cogen[String].contramap(_.displayValue())

  implicit val DCConfigCogen: Cogen[DCConfig] =
    Cogen[(ReadMode, Roi, Int, ExposureTime, Int)].contramap(x =>
      (x.readMode, x.roi, x.coadds, x.exposureTime, x.numberOfFowSamples))

  implicit val CCConfigArb: Arbitrary[CCConfig] =
    Arbitrary {
      for {
        filter       <- arbitrary[Filter]
        odgwSize     <- arbitrary[OdgwSize]
        utilityWheel <- arbitrary[UtilityWheel]
        windowCover  <- arbitrary[WindowCover]
      } yield CCConfig(filter, odgwSize, utilityWheel, windowCover)
    }

  implicit val filterCogen: Cogen[Filter] =
    Cogen[String].contramap(_.displayValue())

  implicit val odgwSizeCogen: Cogen[OdgwSize] =
    Cogen[String].contramap(_.displayValue())

  implicit val utilityWheelCogen: Cogen[UtilityWheel] =
    Cogen[String].contramap(_.displayValue())

  implicit val CCConfigCogen: Cogen[CCConfig] =
    Cogen[(Filter, OdgwSize, UtilityWheel, WindowCover)].contramap(x =>
      (x.filter, x.odgwSize, x.utilityWheel, x.windowCover))
}
