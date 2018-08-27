// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import edu.gemini.spModel.gemini.flamingos2.Flamingos2
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
import gem.arb.ArbTime
import gem.arb.ArbAngle._
import gem.arb.ArbEnumerated._
import gem.Observation
import gem.enum.KeywordName
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import seqexec.server.flamingos2.Flamingos2Controller
import seqexec.server.gpi.GPIController
import seqexec.server.gpi.GPIController._
import seqexec.server.gcal.GcalController
import seqexec.server.gcal.GcalController._
import seqexec.server.tcs.{CRFollow, TcsController, TcsControllerEpics}
import seqexec.server.keywords._
import seqexec.model.enum.Instrument
import seqexec.model.Conditions
import seqexec.model.Operator
import edu.gemini.spModel.gemini.flamingos2.Flamingos2
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
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
import gem.math.{Angle, HourAngle}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import squants.space.LengthConversions._

import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import seqexec.model.SeqexecModelArbitraries._
import seqexec.server.ghost.GHOSTController
import seqexec.server.ghost.GHOSTController.GHOSTConfig

object SeqexecServerArbitraries extends ArbTime {

  implicit val observeCommandArb: Arbitrary[ObserveCommand.Result] = Arbitrary(Gen.oneOf(ObserveCommand.Success, ObserveCommand.Paused, ObserveCommand.Aborted, ObserveCommand.Stopped))
  implicit val observeCommandCogen: Cogen[ObserveCommand.Result] =
    Cogen[String].contramap(_.productPrefix)
  implicit val binaryYNArb: Arbitrary[BinaryYesNo] = Arbitrary(Gen.oneOf(BinaryYesNo.Yes, BinaryYesNo.No))
  implicit val binaryYNCommandCogen: Cogen[BinaryYesNo] =
    Cogen[String].contramap(_.name)
  implicit val binaryOOArb: Arbitrary[BinaryOnOff] = Arbitrary(Gen.oneOf(BinaryOnOff.Off, BinaryOnOff.On))
  implicit val binaryOOCommandCogen: Cogen[BinaryOnOff] =
    Cogen[String].contramap(_.name)
  implicit val tcsBeamArb: Arbitrary[TcsController.Beam] = Arbitrary(Gen.oneOf(TcsController.Beam.A, TcsController.Beam.B, TcsController.Beam.C))
  implicit val tcsBeamCogen: Cogen[TcsController.Beam] =
    Cogen[String].contramap(_.productPrefix)
  implicit val tcsNodChopArb: Arbitrary[TcsController.NodChop] = Arbitrary {
    for {
      n <- arbitrary[TcsController.Beam]
      c <- arbitrary[TcsController.Beam]
    } yield TcsController.NodChop(n, c)
  }
  implicit val tcsNodChopCogen: Cogen[TcsController.NodChop] =
    Cogen[(TcsController.Beam, TcsController.Beam)].contramap(x => (x.nod, x.chop))

  private val lengthMMGen = Gen.posNum[Double].map(_.millimeters)
  implicit val offsetXArb: Arbitrary[TcsController.OffsetX] = Arbitrary(lengthMMGen.map(TcsController.OffsetX.apply))
  implicit val offsetXCogen: Cogen[TcsController.OffsetX] =
    Cogen[Double].contramap(_.self.value)
  implicit val offsetYArb: Arbitrary[TcsController.OffsetY] = Arbitrary(lengthMMGen.map(TcsController.OffsetY.apply))
  implicit val offsetYCogen: Cogen[TcsController.OffsetY] =
    Cogen[Double].contramap(_.self.value)
  implicit val fpoArb: Arbitrary[TcsController.FocalPlaneOffset] = Arbitrary {
    for {
      x <- arbitrary[TcsController.OffsetX]
      y <- arbitrary[TcsController.OffsetY]
    } yield TcsController.FocalPlaneOffset(x, y)
  }
  implicit val fpoCogen: Cogen[TcsController.FocalPlaneOffset] =
    Cogen[(TcsController.OffsetX, TcsController.OffsetY)].contramap(x => (x.x, x.y))
  implicit val offsetAArb: Arbitrary[TcsController.OffsetA] = Arbitrary(arbitrary[TcsController.FocalPlaneOffset].map(TcsController.OffsetA.apply))
  implicit val offsetACogen: Cogen[TcsController.OffsetA] =
    Cogen[TcsController.FocalPlaneOffset].contramap(_.self)
  implicit val offsetBArb: Arbitrary[TcsController.OffsetB] = Arbitrary(arbitrary[TcsController.FocalPlaneOffset].map(TcsController.OffsetB.apply))
  implicit val offsetBCogen: Cogen[TcsController.OffsetB] =
    Cogen[TcsController.FocalPlaneOffset].contramap(_.self)
  implicit val offsetCArb: Arbitrary[TcsController.OffsetC] = Arbitrary(arbitrary[TcsController.FocalPlaneOffset].map(TcsController.OffsetC.apply))
  implicit val offsetCCogen: Cogen[TcsController.OffsetC] =
    Cogen[TcsController.FocalPlaneOffset].contramap(_.self)
  implicit val sfInstNameArb: Arbitrary[TcsControllerEpics.CodexScienceFoldPosition.SFInstName] = Arbitrary(Gen.oneOf(TcsControllerEpics.CodexScienceFoldPosition.instNameMap.values.toSeq))
  implicit val sfInstNameCogen: Cogen[TcsControllerEpics.CodexScienceFoldPosition.SFInstName] =
    Cogen[String].contramap(_.self)

  implicit val f2FPUArb: Arbitrary[Flamingos2.FPUnit] = Arbitrary(Gen.oneOf(Flamingos2.FPUnit.values()))
  implicit val f2FPUCogen: Cogen[Flamingos2.FPUnit] =
    Cogen[String].contramap(_.displayValue())
  implicit val f2CFPUArb: Arbitrary[Flamingos2Controller.FocalPlaneUnit] = Arbitrary(Gen.oneOf(Flamingos2Controller.FocalPlaneUnit.Open, Flamingos2Controller.FocalPlaneUnit.GridSub1Pix,
    Flamingos2Controller.FocalPlaneUnit.Grid2Pix, Flamingos2Controller.FocalPlaneUnit.Slit1Pix, Flamingos2Controller.FocalPlaneUnit.Slit2Pix,
    Flamingos2Controller.FocalPlaneUnit.Slit3Pix, Flamingos2Controller.FocalPlaneUnit.Slit4Pix, Flamingos2Controller.FocalPlaneUnit.Slit6Pix, Flamingos2Controller.FocalPlaneUnit.Slit8Pix))
  implicit val f2CFPUCogen: Cogen[Flamingos2Controller.FocalPlaneUnit] =
    Cogen[String].contramap(_.productPrefix)

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

  implicit val gcalLampArb: Arbitrary[GcalController.LampState] = Arbitrary(Gen.oneOf(GcalController.LampState.On, GcalController.LampState.Off))
  implicit val gcalLampCogen: Cogen[GcalController.LampState] =
    Cogen[String].contramap(_.productPrefix)
  implicit val gcalArLampArb: Arbitrary[GcalController.ArLampState] = Arbitrary(arbitrary[GcalController.LampState].map(ArLampState.apply))
  implicit val gcalArLampCogen: Cogen[GcalController.ArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalCuArLampArb: Arbitrary[GcalController.CuArLampState] = Arbitrary(arbitrary[GcalController.LampState].map(CuArLampState.apply))
  implicit val gcalCuArLampCogen: Cogen[GcalController.CuArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalQhLampArb: Arbitrary[GcalController.QHLampState] = Arbitrary(arbitrary[GcalController.LampState].map(QHLampState.apply))
  implicit val gcalQhLampCogen: Cogen[GcalController.QHLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalThArLampArb: Arbitrary[GcalController.ThArLampState] = Arbitrary(arbitrary[GcalController.LampState].map(ThArLampState.apply))
  implicit val gcalThArLampCogen: Cogen[GcalController.ThArLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalXeLampArb: Arbitrary[GcalController.XeLampState] = Arbitrary(arbitrary[GcalController.LampState].map(XeLampState.apply))
  implicit val gcalXeLampCogen: Cogen[GcalController.XeLampState] =
    Cogen[GcalController.LampState].contramap(_.self)
  implicit val gcalIrLampArb: Arbitrary[GcalController.IrLampState] = Arbitrary(arbitrary[GcalController.LampState].map(IrLampState.apply))
  implicit val gcalIrLampCogen: Cogen[GcalController.IrLampState] =
    Cogen[GcalController.LampState].contramap(_.self)

  implicit val selectedCoGen: Cogen[Map[Instrument, Observation.Id]] =
    Cogen[List[(Instrument, Observation.Id)]].contramap(_.toList)
  implicit val engineMetadataArb: Arbitrary[EngineState] = Arbitrary {
    for {
      q <- arbitrary[ExecutionQueues]
      s <- arbitrary[Map[Instrument, Observation.Id]]
      c <- arbitrary[Conditions]
      o <- arbitrary[Option[Operator]]
    } yield EngineState.default.copy(queues = q, selected = s, conditions = c, operator = o)
  }

  implicit val gpiAOFlagsArb: Arbitrary[GPIController.AOFlags] = Arbitrary{
    for {
      useAo    <- arbitrary[Boolean]
      useCal   <- arbitrary[Boolean]
      aoOpt    <- arbitrary[Boolean]
      alignFpm <- arbitrary[Boolean]
      magH     <- arbitrary[Double]
      magI     <- arbitrary[Double]
    } yield AOFlags(useAo, useCal, aoOpt, alignFpm, magH, magI)
  }
  implicit val gpiAOFlagsCogen: Cogen[GPIController.AOFlags] =
    Cogen[(Boolean, Boolean, Boolean, Boolean)]
      .contramap(x => (x.useAo, x.useCal, x.aoOptimize, x.alignFpm))

  implicit val gpiArtificialSourcesArb: Arbitrary[GPIController.ArtificialSources] = Arbitrary {
    for {
      ir  <- arbitrary[LegacyArtificialSource]
      vis <- arbitrary[LegacyArtificialSource]
      sc  <- arbitrary[LegacyArtificialSource]
      att <- arbitrary[Double]
    } yield ArtificialSources(ir, vis, sc, att)
  }
  implicit val asCogen: Cogen[LegacyArtificialSource] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiArtificialSourcesCogen: Cogen[GPIController.ArtificialSources] =
          Cogen[(LegacyArtificialSource, LegacyArtificialSource, LegacyArtificialSource, Double)]
      .contramap(x => (x.ir, x.vis, x.sc, x.attenuation))

  implicit val gpiShuttersArb: Arbitrary[GPIController.Shutters] = Arbitrary {
    for {
      ent <- arbitrary[LegacyShutter]
      cal <- arbitrary[LegacyShutter]
      sci <- arbitrary[LegacyShutter]
      ref <- arbitrary[LegacyShutter]
    } yield Shutters(ent, cal, sci, ref)
  }
  implicit val shutCogen: Cogen[LegacyShutter] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiShuttersCogen: Cogen[GPIController.Shutters] =
    Cogen[(LegacyShutter, LegacyShutter, LegacyShutter, LegacyShutter)]
      .contramap(x => (x.entranceShutter, x.calEntranceShutter, x.calScienceShutter, x.calReferenceShutter))
  implicit val gpiNonStandardModParamsArb: Arbitrary[NonStandardModeParams] = Arbitrary {
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

  implicit val gpiConfigArb: Arbitrary[GPIController.GPIConfig] = Arbitrary {
    for {
      adc   <- arbitrary[LegacyAdc]
      exp   <- arbitrary[Duration]
      coa   <- Gen.posNum[Int]
      mode  <- arbitrary[Either[LegacyObservingMode, NonStandardModeParams]]
      disp  <- arbitrary[LegacyDisperser]
      dispA <- arbitrary[Double]
      shut  <- arbitrary[GPIController.Shutters]
      asu   <- arbitrary[GPIController.ArtificialSources]
      pc    <- arbitrary[LegacyPupilCamera]
      ao    <- arbitrary[GPIController.AOFlags]
    } yield GPIConfig(adc, exp, coa, mode, disp, dispA, shut, asu, pc, ao)
  }

  implicit val adcCogen: Cogen[LegacyAdc] =
    Cogen[String].contramap(_.displayValue)
  implicit val obsModeCogen: Cogen[LegacyObservingMode] =
    Cogen[String].contramap(_.displayValue)
  implicit val ppCogen: Cogen[LegacyPupilCamera] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiConfigCogen: Cogen[GPIController.GPIConfig] =
    Cogen[(LegacyAdc, Duration, Int, Either[LegacyObservingMode, NonStandardModeParams], GPIController.Shutters, GPIController.ArtificialSources, LegacyPupilCamera, GPIController.AOFlags)]
      .contramap(x => (x.adc, x.expTime, x.coAdds, x.mode, x.shutters, x.asu, x.pc, x.aoFlags))

  implicit val ghostConfigArb: Arbitrary[GHOSTController.GHOSTConfig] = Arbitrary {
    for {
      baseRA <- arbitrary[HourAngle]
      baseDec <- arbitrary[Angle]
      srifu1name <- arbitrary[String]
      srifu1RA <- arbitrary[HourAngle]
      srifu1Dec <- arbitrary[Angle]
      srifu2name <- arbitrary[String]
      srifu2RA <- arbitrary[HourAngle]
      srifu2Dec <- arbitrary[Angle]
    } yield GHOSTConfig(Some(baseRA), Some(baseDec), 60.seconds,
      Some(srifu1name), Some(srifu1RA), Some(srifu1Dec),
      Some(srifu2name), Some(srifu1RA), Some(srifu1Dec),
      None, None, None,
      None, None)
    }

  implicit val ghostCogen: Cogen[GHOSTController.GHOSTConfig] =
    Cogen[(Option[HourAngle], Option[Angle], Duration,
      Option[String], Option[HourAngle], Option[Angle],
      Option[String], Option[HourAngle], Option[Angle],
      Option[String], Option[HourAngle], Option[Angle],
      Option[HourAngle], Option[Angle])]
    .contramap(x => (x.baseRAHMS, x.baseDecDMS, x.expTime,
      x.srifu2Name, x.srifu1CoordsRAHMS, x.srifu1CoordsDecDMS,
      x.srifu2Name, x.srifu2CoordsRAHMS, x.srifu2CoordsDecDMS,
      x.hrifu1Name, x.hrifu1CoordsRAHMS, x.hrifu1CoordsDecDMS,
      x.hrifu2CoordsRAHMS, x.hrifu2CoordsDecDMS
    ))

  implicit val keywordTypeArb: Arbitrary[KeywordType] = Arbitrary {
    Gen.oneOf(TypeInt8, TypeInt16, TypeInt32, TypeFloat, TypeDouble, TypeBoolean, TypeString)
  }
  implicit val keywordTypeCogen: Cogen[KeywordType] =
    Cogen[String].contramap(_.productPrefix)

  implicit val internalKeywordArb: Arbitrary[InternalKeyword] = Arbitrary {
    for {
      name  <- arbitrary[KeywordName]
      kt    <- arbitrary[KeywordType]
      value <- Gen.listOfN(17, Gen.alphaChar)
    } yield InternalKeyword(name, kt, value.mkString)
  }
  implicit val internalKeywordCogen: Cogen[InternalKeyword] =
    Cogen[(KeywordName, KeywordType, String)].contramap(x => (x.name, x.keywordType, x.value))

  implicit val keywordBagArb: Arbitrary[KeywordBag] = Arbitrary {
    arbitrary[List[InternalKeyword]].map(KeywordBag.apply)
  }
  implicit val keywordBagCogen: Cogen[KeywordBag] =
    Cogen[List[InternalKeyword]].contramap(_.keywords)

  implicit val crFollowArb: Arbitrary[CRFollow] = Arbitrary {
    Gen.oneOf(CRFollow.On, CRFollow.Off)
  }
  implicit val crFollowCogen: Cogen[CRFollow] =
    Cogen[String].contramap(_.productPrefix)
}
