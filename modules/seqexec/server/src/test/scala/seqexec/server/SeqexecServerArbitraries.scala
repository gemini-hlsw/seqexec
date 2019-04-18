// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import edu.gemini.spModel.gemini.flamingos2.Flamingos2
import edu.gemini.spModel.gemini.gnirs.GNIRSParams
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
import gem.arb.ArbTime
import gem.arb.ArbCoordinates._
import gem.arb.ArbEnumerated._
import gem.Observation
import gem.enum.KeywordName
import gem.math.Coordinates
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import scala.concurrent.duration.Duration
import scala.concurrent.duration._
import seqexec.model.enum.{BatchCommandState, Instrument}
import seqexec.model.{Conditions, Operator}
import seqexec.model.SeqexecModelArbitraries._
import seqexec.server.flamingos2.Flamingos2Controller
import seqexec.server.gpi.GpiConfig
import seqexec.server.gcal.GcalController
import seqexec.server.gcal.GcalController._
import seqexec.server.tcs.{CRFollow, TcsController}
import seqexec.server.keywords._
import seqexec.server.flamingos2.Flamingos2Controller.ExposureTime
import seqexec.server.ghost.GhostConfig
import shapeless.tag
import shapeless.tag.@@
import squants.Angle
import squants.space.Degrees

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

  private val angleMMGen = Gen.posNum[Double].map(Degrees(_))
  implicit val offsetPArb: Arbitrary[Angle@@TcsController.OffsetP] = Arbitrary(angleMMGen.map(tag[TcsController.OffsetP].apply))
  implicit val offsetPCogen: Cogen[Angle@@TcsController.OffsetP] =
    Cogen[Double].contramap(_.value)
  implicit val offsetYArb: Arbitrary[Angle@@TcsController.OffsetQ] = Arbitrary(angleMMGen.map(tag[TcsController.OffsetQ].apply))
  implicit val offsetYCogen: Cogen[Angle@@TcsController.OffsetQ] =
    Cogen[Double].contramap(_.value)
  implicit val fpoArb: Arbitrary[TcsController.InstrumentOffset] = Arbitrary {
    for {
      p <- arbitrary[Angle@@TcsController.OffsetP]
      q <- arbitrary[Angle@@TcsController.OffsetQ]
    } yield TcsController.InstrumentOffset(p, q)
  }
  implicit val fpoCogen: Cogen[TcsController.InstrumentOffset] =
    Cogen[(Angle@@TcsController.OffsetP, Angle@@TcsController.OffsetQ)].contramap(x => (x.p, x.q))

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
  implicit val engineStateArb: Arbitrary[EngineState] = Arbitrary {
    for {
      q <- arbitrary[ExecutionQueues]
      s <- arbitrary[Map[Instrument, Observation.Id]]
      c <- arbitrary[Conditions]
      o <- arbitrary[Option[Operator]]
    } yield EngineState.default.copy(queues = q, selected = s, conditions = c, operator = o)
  }

  implicit val gpiAOFlagsArb: Arbitrary[gpi.AOFlags] = Arbitrary{
    for {
      useAo    <- arbitrary[Boolean]
      useCal   <- arbitrary[Boolean]
      aoOpt    <- arbitrary[Boolean]
      alignFpm <- arbitrary[Boolean]
      magH     <- arbitrary[Double]
      magI     <- arbitrary[Double]
    } yield gpi.AOFlags(useAo, useCal, aoOpt, alignFpm, magH, magI)
  }
  implicit val gpiAOFlagsCogen: Cogen[gpi.AOFlags] =
    Cogen[(Boolean, Boolean, Boolean, Boolean)]
      .contramap(x => (x.useAo, x.useCal, x.aoOptimize, x.alignFpm))

  implicit val gpiArtificialSourcesArb: Arbitrary[gpi.ArtificialSources] = Arbitrary {
    for {
      ir  <- arbitrary[LegacyArtificialSource]
      vis <- arbitrary[LegacyArtificialSource]
      sc  <- arbitrary[LegacyArtificialSource]
      att <- arbitrary[Double]
    } yield gpi.ArtificialSources(ir, vis, sc, att)
  }
  implicit val asCogen: Cogen[LegacyArtificialSource] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiArtificialSourcesCogen: Cogen[gpi.ArtificialSources] =
          Cogen[(LegacyArtificialSource, LegacyArtificialSource, LegacyArtificialSource, Double)]
      .contramap(x => (x.ir, x.vis, x.sc, x.attenuation))

  implicit val gpiShuttersArb: Arbitrary[gpi.Shutters] = Arbitrary {
    for {
      ent <- arbitrary[LegacyShutter]
      cal <- arbitrary[LegacyShutter]
      sci <- arbitrary[LegacyShutter]
      ref <- arbitrary[LegacyShutter]
    } yield gpi.Shutters(ent, cal, sci, ref)
  }
  implicit val shutCogen: Cogen[LegacyShutter] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiShuttersCogen: Cogen[gpi.Shutters] =
    Cogen[(LegacyShutter, LegacyShutter, LegacyShutter, LegacyShutter)]
      .contramap(x => (x.entranceShutter, x.calEntranceShutter, x.calScienceShutter, x.calReferenceShutter))
  implicit val gpiNonStandardModParamsArb: Arbitrary[gpi.NonStandardModeParams] = Arbitrary {
    for {
      apo <- arbitrary[LegacyApodizer]
      fpm <- arbitrary[LegacyFPM]
      lyo <- arbitrary[LegacyLyot]
      fil <- arbitrary[LegacyFilter]
    } yield gpi.NonStandardModeParams(apo, fpm, lyo, fil)
  }
  implicit val apodizerCogen: Cogen[LegacyApodizer] =
    Cogen[String].contramap(_.displayValue)
  implicit val fpmCogen: Cogen[LegacyFPM] =
    Cogen[String].contramap(_.displayValue)
  implicit val lyotCogen: Cogen[LegacyLyot] =
    Cogen[String].contramap(_.displayValue)
  implicit val filterCogen: Cogen[LegacyFilter] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiNonStandardModeCogen: Cogen[gpi.NonStandardModeParams] =
    Cogen[(LegacyApodizer, LegacyFPM, LegacyLyot, LegacyFilter)]
      .contramap(x => (x.apodizer, x.fpm, x.lyot, x.filter))

  implicit val gpiConfigArb: Arbitrary[gpi.GpiConfig] = Arbitrary {
    for {
      adc   <- arbitrary[LegacyAdc]
      exp   <- arbitrary[Duration]
      coa   <- Gen.posNum[Int]
      mode  <- arbitrary[Either[LegacyObservingMode, gpi.NonStandardModeParams]]
      disp  <- arbitrary[LegacyDisperser]
      dispA <- arbitrary[Double]
      shut  <- arbitrary[gpi.Shutters]
      asu   <- arbitrary[gpi.ArtificialSources]
      pc    <- arbitrary[LegacyPupilCamera]
      ao    <- arbitrary[gpi.AOFlags]
    } yield GpiConfig(adc, exp, coa, mode, disp, dispA, shut, asu, pc, ao)
  }

  implicit val adcCogen: Cogen[LegacyAdc] =
    Cogen[String].contramap(_.displayValue)
  implicit val obsModeCogen: Cogen[LegacyObservingMode] =
    Cogen[String].contramap(_.displayValue)
  implicit val ppCogen: Cogen[LegacyPupilCamera] =
    Cogen[String].contramap(_.displayValue)
  implicit val gpiConfigCogen: Cogen[gpi.GpiConfig] =
    Cogen[(LegacyAdc, Duration, Int, Either[LegacyObservingMode, gpi.NonStandardModeParams], gpi.Shutters, gpi.ArtificialSources, LegacyPupilCamera, gpi.AOFlags)]
      .contramap(x => (x.adc, x.expTime, x.coAdds, x.mode, x.shutters, x.asu, x.pc, x.aoFlags))

  val ghostSRSingleTargetConfigGen: Gen[ghost.StandardResolutionMode.SingleTarget] =
    for {
      basePos <- arbitrary[Option[Coordinates]]
      exp <- arbitrary[ExposureTime]
      srifu1Name <- arbitrary[String]
      srifu1Pos <- arbitrary[Coordinates]
    } yield ghost.StandardResolutionMode.SingleTarget(basePos, exp, srifu1Name, srifu1Pos)

  implicit val ghostSRSingleTargetConfigCogen: Cogen[ghost.StandardResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], ExposureTime, String, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

  val ghostSRDualTargetConfigGen: Gen[ghost.StandardResolutionMode.DualTarget] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[ExposureTime]
      srifu1Name <- arbitrary[String]
      srifu1Pos  <- arbitrary[Coordinates]
      srifu2Name <- arbitrary[String]
      srifu2Pos  <- arbitrary[Coordinates]
    } yield ghost.StandardResolutionMode.DualTarget(basePos, exp, srifu1Name, srifu1Pos, srifu2Name, srifu2Pos)

  implicit val ghostSRDualTargetConfigCogen: Cogen[ghost.StandardResolutionMode.DualTarget] =
    Cogen[(Option[Coordinates], ExposureTime, String, Coordinates, String, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2TargetName, x.ifu2Coordinates))

  val ghostSRTargetSkyConfigGen: Gen[ghost.StandardResolutionMode.TargetPlusSky] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[ExposureTime]
      srifu1Name <- arbitrary[String]
      srifu1Pos  <- arbitrary[Coordinates]
      srifu2Pos  <- arbitrary[Coordinates]
    } yield ghost.StandardResolutionMode.TargetPlusSky(basePos, exp, srifu1Name, srifu1Pos, srifu2Pos)

  implicit val ghostSRTargetSkyConfigCogen: Cogen[ghost.StandardResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], ExposureTime, String, Coordinates, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates))

  implicit val ghostSRSkyTargetConfigGen: Gen[ghost.StandardResolutionMode.SkyPlusTarget] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[ExposureTime]
      srifu1Pos  <- arbitrary[Coordinates]
      srifu2Name <- arbitrary[String]
      srifu2Pos  <- arbitrary[Coordinates]
    } yield ghost.StandardResolutionMode.SkyPlusTarget(basePos, exp, srifu1Pos, srifu2Name, srifu2Pos)

  implicit val ghostSRSkyTargetConfigCogen: Cogen[ghost.StandardResolutionMode.SkyPlusTarget] =
    Cogen[(Option[Coordinates], ExposureTime, Coordinates, String, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1Coordinates, x.ifu2TargetName, x.ifu2Coordinates))

  implicit val ghostHRSingleTargetConfigGen: Gen[ghost.HighResolutionMode.SingleTarget] =
    for {
      basePos   <- arbitrary[Option[Coordinates]]
      exp       <- arbitrary[ExposureTime]
      hrifu1Name <- arbitrary[String]
      hrifu1Pos <- arbitrary[Coordinates]
    } yield ghost.HighResolutionMode.SingleTarget(basePos, exp, hrifu1Name, hrifu1Pos)

  implicit val ghostHRSingleTargetConfigCogen: Cogen[ghost.HighResolutionMode.SingleTarget] =
    Cogen[(Option[Coordinates], ExposureTime, String, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates))

  implicit val ghostHRTargetPlusSkyConfigGen: Gen[ghost.HighResolutionMode.TargetPlusSky] =
    for {
      basePos    <- arbitrary[Option[Coordinates]]
      exp        <- arbitrary[ExposureTime]
      hrifu1Name <- arbitrary[String]
      hrifu1Pos  <- arbitrary[Coordinates]
      hrifu2Pos  <- arbitrary[Coordinates]
    } yield ghost.HighResolutionMode.TargetPlusSky(basePos, exp, hrifu1Name, hrifu1Pos, hrifu2Pos)

  implicit val ghostHRTargetSkyConfigCogen: Cogen[ghost.HighResolutionMode.TargetPlusSky] =
    Cogen[(Option[Coordinates], ExposureTime, String, Coordinates, Coordinates)]
      .contramap(x => (x.baseCoords, x.expTime, x.ifu1TargetName, x.ifu1Coordinates, x.ifu2Coordinates))

  implicit val ghostConfigArb: Arbitrary[GhostConfig] = Arbitrary {
    Gen.oneOf(
      ghostSRSingleTargetConfigGen,
      ghostSRDualTargetConfigGen,
      ghostSRTargetSkyConfigGen,
      ghostSRSkyTargetConfigGen,
      ghostHRSingleTargetConfigGen,
      ghostHRTargetPlusSkyConfigGen)
  }

  object GhostHelpers {
    def extractSRIFU1Name(x: ghost.GhostConfig): Option[String] = x match {
      case ghost.StandardResolutionMode.SingleTarget(_, _, name, _)     => Some(name)
      case ghost.StandardResolutionMode.DualTarget(_, _, name, _, _, _) => Some(name)
      case ghost.StandardResolutionMode.TargetPlusSky(_, _, name, _, _) => Some(name)
      case _: ghost.StandardResolutionMode.SkyPlusTarget                => Some("Sky")
      case _                                                                      => None
    }

    def extractSRIFU1Coordinates(x: ghost.GhostConfig): Option[Coordinates] = x match {
      case c: ghost.StandardResolutionMode => Some(c.ifu1Coordinates)
      case _                                         => None
    }

    def extractSRIFU2Name(x: ghost.GhostConfig): Option[String] = x match {
      case ghost.StandardResolutionMode.DualTarget(_, _, _, _, name, _) => Some(name)
      case _: ghost.StandardResolutionMode.TargetPlusSky                => Some("Sky")
      case ghost.StandardResolutionMode.SkyPlusTarget(_, _, _, name, _) => Some(name)
      case _                                                                      => None
    }

    def extractSRIFU2Coordinates(x: ghost.GhostConfig): Option[Coordinates] = x match {
      case ghost.StandardResolutionMode.DualTarget(_, _, _, _, _, coords) => Some(coords)
      case ghost.StandardResolutionMode.TargetPlusSky(_, _, _, _, coords) => Some(coords)
      case ghost.StandardResolutionMode.SkyPlusTarget(_, _, _, _, coords) => Some(coords)
      case _                                                                        => None
    }

    def extractHRIFU1Name(x: ghost.GhostConfig): Option[String] = x match {
      case c: ghost.HighResolutionMode => Some(c.ifu1TargetName)
      case _                                     => None
    }

    def extractHRIFU1Coordinates(x: ghost.GhostConfig): Option[Coordinates] = x match {
      case c: ghost.HighResolutionMode => Some(c.ifu1Coordinates)
      case _                                     => None
    }

    def extractHRIFU2Name(x: ghost.GhostConfig): Option[String] = x match {
      case _: ghost.HighResolutionMode.TargetPlusSky => Some("Sky")
      case _                                                   => None
    }

    def extractHRIFU2Coordinates(x: ghost.GhostConfig): Option[Coordinates] = x match {
      case c: ghost.HighResolutionMode.TargetPlusSky => Some(c.ifu2Coordinates)
      case _                                                   => None
    }
  }

  implicit val ghostConfigCoGen: Cogen[ghost.GhostConfig] = {
    import GhostHelpers._
    Cogen[(Option[Coordinates],
      Duration,
      Option[String],
      Option[Coordinates],
      Option[String],
      Option[Coordinates],
      Option[String],
      Option[Coordinates],
      Option[String],
      Option[Coordinates])]
      .contramap(
        x => (x.baseCoords,
          x.expTime,
          extractSRIFU1Name(x),
          extractSRIFU1Coordinates(x),
          extractSRIFU2Name(x),
          extractSRIFU2Coordinates(x),
          extractHRIFU1Name(x),
          extractHRIFU1Coordinates(x),
          extractHRIFU2Name(x),
          extractHRIFU2Coordinates(x)
        ))}


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

  implicit val executionQueueArb: Arbitrary[ExecutionQueue] = Arbitrary {
    for {
      n <- arbitrary[String]
      s <- arbitrary[BatchCommandState]
      q <- arbitrary[List[Observation.Id]]
    } yield ExecutionQueue(n, s, q)
  }

  implicit val executionQueueCogen: Cogen[ExecutionQueue] =
    Cogen[(String, BatchCommandState, List[Observation.Id])]
      .contramap(x => (x.name, x.cmdState, x.queue))
}
