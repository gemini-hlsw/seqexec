// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import edu.gemini.spModel.gemini.gpi.Gpi.{Apodizer => LegacyApodizer}
import edu.gemini.spModel.gemini.gpi.Gpi.{Adc => LegacyAdc}
import edu.gemini.spModel.gemini.gpi.Gpi.{ArtificialSource => LegacyArtificialSource}
import edu.gemini.spModel.gemini.gpi.Gpi.{Disperser => LegacyDisperser}
import edu.gemini.spModel.gemini.gpi.Gpi.{FPM => LegacyFPM}
import edu.gemini.spModel.gemini.gpi.Gpi.{Filter => LegacyFilter}
import edu.gemini.spModel.gemini.gpi.Gpi.{Lyot => LegacyLyot}
import edu.gemini.spModel.gemini.gpi.Gpi.{ObservingMode => LegacyObservingMode}
import edu.gemini.spModel.gemini.gpi.Gpi.{PupilCamera => LegacyPupilCamera}
import edu.gemini.spModel.gemini.gpi.Gpi.{Shutter => LegacyShutter}
import gem.arb.ArbTime
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import scala.concurrent.duration.Duration

trait GpiArbitraries extends ArbTime {

  implicit val gpiAOFlagsArb: Arbitrary[AOFlags] = Arbitrary {
    for {
      useAo    <- arbitrary[Boolean]
      useCal   <- arbitrary[Boolean]
      aoOpt    <- arbitrary[Boolean]
      alignFpm <- arbitrary[Boolean]
      magH     <- arbitrary[Double]
      magI     <- arbitrary[Double]
    } yield AOFlags(useAo, useCal, aoOpt, alignFpm, magH, magI)
  }
  implicit val gpiAOFlagsCogen: Cogen[AOFlags] =
    Cogen[(Boolean, Boolean, Boolean, Boolean)]
      .contramap(x => (x.useAo, x.useCal, x.aoOptimize, x.alignFpm))

  implicit val gpiArtificialSourcesArb: Arbitrary[ArtificialSources] =
    Arbitrary {
      for {
        ir  <- arbitrary[LegacyArtificialSource]
        vis <- arbitrary[LegacyArtificialSource]
        sc  <- arbitrary[LegacyArtificialSource]
        att <- arbitrary[Double]
      } yield ArtificialSources(ir, vis, sc, att)
    }
  implicit val asCogen: Cogen[LegacyArtificialSource] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiArtificialSourcesCogen: Cogen[ArtificialSources] =
    Cogen[(LegacyArtificialSource,
           LegacyArtificialSource,
           LegacyArtificialSource,
           Double)]
      .contramap(x => (x.ir, x.vis, x.sc, x.attenuation))

  implicit val gpiShuttersArb: Arbitrary[Shutters] = Arbitrary {
    for {
      ent <- arbitrary[LegacyShutter]
      cal <- arbitrary[LegacyShutter]
      sci <- arbitrary[LegacyShutter]
      ref <- arbitrary[LegacyShutter]
    } yield Shutters(ent, cal, sci, ref)
  }

  implicit val shutCogen: Cogen[LegacyShutter] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiShuttersCogen: Cogen[Shutters] =
    Cogen[(LegacyShutter, LegacyShutter, LegacyShutter, LegacyShutter)]
      .contramap(
        x =>
          (x.entranceShutter,
           x.calEntranceShutter,
           x.calScienceShutter,
           x.calReferenceShutter))

  implicit val gpiNonStandardModParamsArb: Arbitrary[NonStandardModeParams] =
    Arbitrary {
      for {
        apo <- arbitrary[LegacyApodizer]
        fpm <- arbitrary[LegacyFPM]
        lyo <- arbitrary[LegacyLyot]
        fil <- arbitrary[LegacyFilter]
      } yield NonStandardModeParams(apo, fpm, lyo, fil)
    }
  implicit val apodizerCogen: Cogen[LegacyApodizer] =
    Cogen[String].contramap(_.displayValue)
  implicit val fpmCogen: Cogen[LegacyFPM] =
    Cogen[String].contramap(_.displayValue)
  implicit val lyotCogen: Cogen[LegacyLyot] =
    Cogen[String].contramap(_.displayValue)
  implicit val filterCogen: Cogen[LegacyFilter] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiNonStandardModeCogen: Cogen[NonStandardModeParams] =
    Cogen[(LegacyApodizer, LegacyFPM, LegacyLyot, LegacyFilter)]
      .contramap(x => (x.apodizer, x.fpm, x.lyot, x.filter))

  implicit val gpiConfigArb: Arbitrary[GpiConfig] = Arbitrary {
    for {
      adc   <- arbitrary[LegacyAdc]
      exp   <- arbitrary[Duration]
      coa   <- Gen.posNum[Int]
      mode  <- arbitrary[Either[LegacyObservingMode, NonStandardModeParams]]
      disp  <- arbitrary[LegacyDisperser]
      dispA <- arbitrary[Double]
      shut  <- arbitrary[Shutters]
      asu   <- arbitrary[ArtificialSources]
      pc    <- arbitrary[LegacyPupilCamera]
      ao    <- arbitrary[AOFlags]
    } yield GpiConfig(adc, exp, coa, mode, disp, dispA, shut, asu, pc, ao)
  }

  implicit val adcCogen: Cogen[LegacyAdc] =
    Cogen[String].contramap(_.displayValue)
  implicit val obsModeCogen: Cogen[LegacyObservingMode] =
    Cogen[String].contramap(_.displayValue)
  implicit val ppCogen: Cogen[LegacyPupilCamera] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiConfigCogen: Cogen[GpiConfig] =
    Cogen[(LegacyAdc,
       Duration,
       Int,
       Either[LegacyObservingMode, NonStandardModeParams],
       Shutters,
       ArtificialSources,
       LegacyPupilCamera,
       AOFlags)]
      .contramap(
        x =>
          (x.adc,
           x.expTime,
           x.coAdds,
           x.mode,
           x.shutters,
           x.asu,
           x.pc,
           x.aoFlags))
}
