package edu.gemini.seqexec.server

import java.util
import java.util.logging.Logger
import edu.gemini.seqexec.server.tcs.{BinaryYesNo, BinaryOnOff}
import squants.time.Seconds

import collection.JavaConversions._

import edu.gemini.seqexec.server.TcsController._
import edu.gemini.epics.acm.{CaAttributeListener, CaService, XMLBuilder}
import edu.gemini.spModel.core.Wavelength
import squants.space.{Degrees, Microns, Millimeters}

import scalaz._
import Scalaz._
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 9/7/15.
 */
object TcsControllerEpics extends TcsController {
  private val Log = Logger.getLogger(getClass.getName)

  import EpicsCodex._

  // Code to retrieve the current configuration from TCS. Include a lot of decoders
  implicit private val decodeMountGuideOption: DecodeEpicsValue[Integer, MountGuideOption] = DecodeEpicsValue((d: Integer)
  => if (d == 0) MountGuideOff else MountGuideOn)

  implicit private val decodeM1GuideSource: DecodeEpicsValue[String, M1Source] = DecodeEpicsValue((s: String)
  => s.trim match {
      case "PWFS1" => M1Source.PWFS1
      case "PWFS2" => M1Source.PWFS2
      case "OIWFS" => M1Source.OIWFS
      case "GAOS" => M1Source.GAOS
      case _ => M1Source.PWFS1
    })

  private def decodeM1Guide(r: BinaryOnOff, s: M1Source): M1GuideConfig =
    if (r == BinaryOnOff.Off) M1GuideOff
    else M1GuideOn(s)

  private def decodeGuideSourceOption(s: String): Boolean = s.trim == "ON"

  implicit private val decodeComaOption: DecodeEpicsValue[String, ComaOption] = DecodeEpicsValue((s: String)
  => if (s.trim == "Off") ComaOff else ComaOn)

  private def decodeM2Guide(s: BinaryOnOff, u: ComaOption, v: Set[TipTiltSource]): M2GuideConfig =
    if (s == BinaryOnOff.Off) M2GuideOff
    else M2GuideOn(u, v)

  private def getGuideConfig: TrySeq[GuideConfig] = {
    for {
      mountGuide <- TcsEpics().absorbTipTilt.map(decode[Integer, MountGuideOption])
      m1Source <- TcsEpics().m1GuideSource.map(decode[String, M1Source])
      m1Guide <- TcsEpics().m1Guide.map(decodeM1Guide(_, m1Source))
      m2p1Guide <- TcsEpics().m2p1Guide.map(decodeGuideSourceOption)
      m2p2Guide <- TcsEpics().m2p2Guide.map(decodeGuideSourceOption)
      m2oiGuide <- TcsEpics().m2oiGuide.map(decodeGuideSourceOption)
      m2aoGuide <- TcsEpics().m2aoGuide.map(decodeGuideSourceOption)
      m2Coma <- TcsEpics().comaCorrect.map(decode[String, ComaOption])
      m2Guide <- TcsEpics().m2GuideState.map(decodeM2Guide(_, m2Coma, List((m2p1Guide, TipTiltSource.PWFS1),
        (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
        (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set[TipTiltSource]())((s: Set[TipTiltSource], v: (Boolean, TipTiltSource)) => if (v._1) s + v._2 else s)))
    } yield TrySeq(GuideConfig(mountGuide, m1Guide, m2Guide))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guide configuration from TCS.")))

  implicit private val decodeBeam: DecodeEpicsValue[String, Beam] = DecodeEpicsValue((op: String)
  => op match {
      case "A" => Beam.A
      case "B" => Beam.B
      case "C" => Beam.C
      case _ => Beam.A
    }
  )

  private def getTelescopeConfig: TrySeq[TelescopeConfig] = {
    for {
      xOffsetA <- TcsEpics().xoffsetPoA1
      yOffsetA <- TcsEpics().yoffsetPoA1
      xOffsetB <- TcsEpics().xoffsetPoB1
      yOffsetB <- TcsEpics().yoffsetPoB1
      xOffsetC <- TcsEpics().xoffsetPoC1
      yOffsetC <- TcsEpics().yoffsetPoC1
      wavelengthA <- TcsEpics().sourceAWavelength
      wavelengthB <- TcsEpics().sourceBWavelength
      wavelengthC <- TcsEpics().sourceCWavelength
      m2Beam <- TcsEpics().chopBeam.map(decode[String, Beam])
    } yield TrySeq(TelescopeConfig(
      OffsetA(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetA)), OffsetY(Millimeters[Double](yOffsetA)))),
      OffsetB(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetB)), OffsetY(Millimeters[Double](yOffsetB)))),
      OffsetC(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetC)), OffsetY(Millimeters[Double](yOffsetC)))),
      WavelengthA(Wavelength(Microns[Double](wavelengthA))),
      WavelengthB(Wavelength(Microns[Double](wavelengthB))),
      WavelengthC(Wavelength(Microns[Double](wavelengthC))),
      m2Beam
    ))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read telescope configuration from TCS.")))

  private def decodeNodChopOption(s: String): Boolean = s.trim == "On"

  private def getNodChopTrackingConfig(g: TcsEpics.ProbeGuideConfig): Option[NodChopTrackingConfig] = {
    for {
      aa <- g.nodachopa.map(decodeNodChopOption)
      ab <- g.nodachopb.map(decodeNodChopOption)
      ac <- g.nodachopc.map(decodeNodChopOption)
      ba <- g.nodbchopa.map(decodeNodChopOption)
      bb <- g.nodbchopb.map(decodeNodChopOption)
      bc <- g.nodbchopc.map(decodeNodChopOption)
      ca <- g.nodcchopa.map(decodeNodChopOption)
      cb <- g.nodcchopb.map(decodeNodChopOption)
      cc <- g.nodcchopc.map(decodeNodChopOption)
    } yield {
      if (List(aa, ab, ac, ba, bb, bc, ca, cb, cc).exists(x => x)) {
        if (List(aa, bb, cc).forall(x => x) && !List(ab, ac, ba, bc, ca, cb).exists(x => x))
          NodChopTrackingConfig.Normal
        else NodChopTrackingConfig.Special(List((aa, NodChop(Beam.A, Beam.A)), (ab, NodChop(Beam.A, Beam.B)),
          (ac, NodChop(Beam.A, Beam.C)), (ba, NodChop(Beam.B, Beam.A)), (bb, NodChop(Beam.B, Beam.B)),
          (bc, NodChop(Beam.B, Beam.C)), (ca, NodChop(Beam.C, Beam.A)), (cb, NodChop(Beam.C, Beam.B)),
          (cc, NodChop(Beam.C, Beam.C))
        ).foldLeft(Set[NodChop]())((s: Set[NodChop], v: (Boolean, NodChop)) => if (v._1) s + v._2 else s))
      }
      else NodChopTrackingConfig.None
    }
  }

  private def calcProbeTrackingConfig(f: FollowOption, t: NodChopTrackingConfig): ProbeTrackingConfig = (f, t) match {
    case (_, NodChopTrackingConfig.None) => ProbeTrackingConfig.Off
    case (FollowOn, NodChopTrackingConfig.Normal) => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
    case (FollowOn, v: NodChopTrackingConfig.Special) => ProbeTrackingConfig.On(v)
    case _ => ProbeTrackingConfig.Off
  }

  implicit private val decodeFollowOption: DecodeEpicsValue[String, FollowOption] = DecodeEpicsValue((s: String)
  => if (s.trim == "Off") FollowOff else FollowOn)

  private def getGuidersTrackingConfig: TrySeq[GuidersTrackingConfig] = {
    for {
      p1 <- getNodChopTrackingConfig(TcsEpics().pwfs1ProbeGuideConfig)
      p2 <- getNodChopTrackingConfig(TcsEpics().pwfs2ProbeGuideConfig)
      oi <- getNodChopTrackingConfig(TcsEpics().oiwfsProbeGuideConfig)
      p1Follow <- TcsEpics().p1FollowS.map(decode[String, FollowOption])
      p2Follow <- TcsEpics().p2FollowS.map(decode[String, FollowOption])
      oiFollow <- TcsEpics().oiFollowS.map(decode[String, FollowOption])
    } yield TrySeq(GuidersTrackingConfig(ProbeTrackingConfigP1(calcProbeTrackingConfig(p1Follow, p1)),
      ProbeTrackingConfigP2(calcProbeTrackingConfig(p2Follow, p2)),
      ProbeTrackingConfigOI(calcProbeTrackingConfig(oiFollow, oi)),
      ProbeTrackingConfigAO(ProbeTrackingConfig.Off)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read probes guide from TCS.")))

  implicit private val decodeGuideSensorOption: DecodeEpicsValue[BinaryYesNo, GuiderSensorOption] = DecodeEpicsValue((s: BinaryYesNo)
  => if (s == BinaryYesNo.No) GuiderSensorOff else GuiderSensorOn)
  implicit private val decodeAltairSensorOption: DecodeEpicsValue[Double, GuiderSensorOption] = DecodeEpicsValue((s: Double)
  => if (s == 0.0) GuiderSensorOff else GuiderSensorOn)

  private def getGuidersEnabled: TrySeq[GuidersEnabled] = {
    for {
      p1On <- TcsEpics().pwfs1On.map(decode[BinaryYesNo, GuiderSensorOption])
      p2On <- TcsEpics().pwfs2On.map(decode[BinaryYesNo, GuiderSensorOption])
      oiOn <- TcsEpics().oiwfsOn.map(decode[BinaryYesNo, GuiderSensorOption])
      aoOn <- TcsEpics().aowfsOn.map(decode[Double, GuiderSensorOption])
    } yield TrySeq(GuidersEnabled(GuiderSensorOptionP1(p1On), GuiderSensorOptionP2(p2On), GuiderSensorOptionOI(oiOn),
      GuiderSensorOptionAO(aoOn)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guider detectors state from TCS.")))

  // Decoding and encoding the science fold position require some common definitions, therefore I put them inside an
  // object
  private object CodexScienceFoldPosition {

    import LightSource._
    import ScienceFoldPosition._

    private val AO_PREFIX = "ao2"
    private val GCAL_PREFIX = "gcal2"

    // I have the feeling this operation will be needed in other places
    private def findInstrument(name: String): Instrument =
      List(GMOS_S).find(x => name.startsWith(x.sfName)).getOrElse(UnknownInstrument)

    implicit val decodeScienceFoldPosition: DecodeEpicsValue[String, Position] = DecodeEpicsValue((t: String)
    => if (t.startsWith(AO_PREFIX)) Position(AO, findInstrument(t.substring(AO_PREFIX.length)))
      else if (t.startsWith(GCAL_PREFIX)) Position(GCAL, findInstrument(t.substring(GCAL_PREFIX.length)))
      else Position(Sky, findInstrument(t)))

    implicit val encodeScienceFoldPosition: EncodeEpicsValue[Position, String] = EncodeEpicsValue((a: Position)
    => a.source match {
        case Sky => a.sink.sfName
        case AO => AO_PREFIX + a.sink.sfName
        case GCAL => GCAL_PREFIX + a.sink.sfName
      }
    )
  }

  import CodexScienceFoldPosition._

  private def getScienceFoldPosition: Option[ScienceFoldPosition] = for {
    sfPos <- TcsEpics().sfName.map(decode[String, ScienceFoldPosition.Position])
    sfParked <- TcsEpics().sfParked.map {
      _ != 0
    }
  } yield if (sfParked) ScienceFoldPosition.Parked
    else sfPos

  implicit val decodeHwrsPickupPosition: DecodeEpicsValue[String, HrwfsPickupPosition] = DecodeEpicsValue((t: String)
  => if (t.trim == "IN") HrwfsPickupPosition.IN
    else HrwfsPickupPosition.OUT)

  private def getHrwfsPickupPosition: Option[HrwfsPickupPosition] = for {
    hwPos <- TcsEpics().agHwName.map(decode[String, HrwfsPickupPosition])
    hwParked <- TcsEpics().agHwParked.map {
      _ != 0
    }
  } yield if (hwParked) HrwfsPickupPosition.Parked
    else hwPos

  private def getAGConfig: TrySeq[AGConfig] = {
    for {
      sf <- getScienceFoldPosition
      hrwfs <- getHrwfsPickupPosition
    } yield TrySeq(AGConfig(sf, hrwfs))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read AG state from TCS.")))

  private def getIAA: TrySeq[InstrumentAlignAngle] = {
    for {
      iaa <- TcsEpics().instrAA
    } yield TrySeq(InstrumentAlignAngle(Degrees[Double](iaa)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read IAA from TCS.")))

  override def getConfig: SeqAction[TcsConfig] = EitherT ( Task {
    for {
      gc <- getGuideConfig
      tc <- getTelescopeConfig
      gtc <- getGuidersTrackingConfig
      ge <- getGuidersEnabled
      agc <- getAGConfig
      iaa <- getIAA
    } yield TcsConfig(gc, tc, gtc, ge, agc, iaa)
  } )

  // Here starts the code that set the TCS configuration. There are a lot of encoders.
  implicit private val encodeBeam: EncodeEpicsValue[Beam, String] = EncodeEpicsValue((op: Beam)
  => op match {
      case Beam.A => "A"
      case Beam.B => "B"
      case Beam.C => "C"
    })

  private def setTelescopeConfig(c: TelescopeConfig): SeqAction[Unit] = for {
    _ <- TcsEpics().offsetACmd.setX(c.offsetA.self.x.self.toMillimeters)
    _ <- TcsEpics().offsetACmd.setY(c.offsetA.self.y.self.toMillimeters)
    _ <- TcsEpics().offsetBCmd.setX(c.offsetB.self.x.self.toMillimeters)
    _ <- TcsEpics().offsetBCmd.setY(c.offsetB.self.y.self.toMillimeters)
    _ <- TcsEpics().offsetCCmd.setX(c.offsetC.self.x.self.toMillimeters)
    _ <- TcsEpics().offsetCCmd.setY(c.offsetC.self.y.self.toMillimeters)
    _ <- TcsEpics().wavelSourceA.setWavel(c.wavelA.self.toMicrons)
    _ <- TcsEpics().wavelSourceA.setWavel(c.wavelB.self.toMicrons)
    _ <- TcsEpics().wavelSourceA.setWavel(c.wavelB.self.toMicrons)
    _ <- TcsEpics().m2Beam.setBeam(encode(c.m2beam))
  } yield TrySeq(())

  implicit private val encodeNodChopOption: EncodeEpicsValue[NodChopTrackingOption, String] = EncodeEpicsValue((op: NodChopTrackingOption)
  => op match {
      case NodChopTrackingOn => "on"
      case NodChopTrackingOff => "off"
    })

  private def setProbeTrackingConfig(s: TcsEpics.ProbeGuideCmd, c: ProbeTrackingConfig) = for {
    _ <- s.setNodachopa(encode(c.getNodChop.get(NodChop(Beam.A, Beam.A))))
    _ <- s.setNodachopb(encode(c.getNodChop.get(NodChop(Beam.A, Beam.B))))
    _ <- s.setNodachopc(encode(c.getNodChop.get(NodChop(Beam.A, Beam.C))))
    _ <- s.setNodbchopa(encode(c.getNodChop.get(NodChop(Beam.B, Beam.A))))
    _ <- s.setNodbchopb(encode(c.getNodChop.get(NodChop(Beam.B, Beam.B))))
    _ <- s.setNodbchopc(encode(c.getNodChop.get(NodChop(Beam.B, Beam.C))))
    _ <- s.setNodcchopa(encode(c.getNodChop.get(NodChop(Beam.C, Beam.A))))
    _ <- s.setNodcchopb(encode(c.getNodChop.get(NodChop(Beam.C, Beam.B))))
    _ <- s.setNodcchopc(encode(c.getNodChop.get(NodChop(Beam.C, Beam.C))))
  } yield TrySeq(())

  private def setGuiderWfs(on: TcsEpics.WfsObserveCmd, off: EpicsCommand, c: GuiderSensorOption): SeqAction[Unit] =
    c match {
      case GuiderSensorOff => off.mark
      case GuiderSensorOn => on.setNoexp(-1) // Set number of exposures to non-stop (-1)
    }

  private def setGuidersWfs(c: GuidersEnabled): SeqAction[Unit] = for {
    _ <- setGuiderWfs(TcsEpics().pwfs1ObserveCmd, TcsEpics().pwfs1StopObserveCmd, c.pwfs1.self)
    _ <- setGuiderWfs(TcsEpics().pwfs2ObserveCmd, TcsEpics().pwfs2StopObserveCmd, c.pwfs2.self)
    _ <- setGuiderWfs(TcsEpics().oiwfsObserveCmd, TcsEpics().oiwfsStopObserveCmd, c.oiwfs.self)
  } yield TrySeq(())

  private def setProbesTrackingConfig(c: GuidersTrackingConfig): SeqAction[Unit] = for {
    _ <- setProbeTrackingConfig(TcsEpics().pwfs1ProbeGuideCmd, c.pwfs1.self)
    _ <- setProbeTrackingConfig(TcsEpics().pwfs2ProbeGuideCmd, c.pwfs2.self)
    _ <- setProbeTrackingConfig(TcsEpics().oiwfsProbeGuideCmd, c.oiwfs.self)
  } yield TrySeq(())

  def setScienceFoldConfig(sfPos: ScienceFoldPosition): SeqAction[Unit] = sfPos match {
    case ScienceFoldPosition.Parked => TcsEpics().scienceFoldParkCmd.mark
    case p: ScienceFoldPosition.Position => TcsEpics().scienceFoldPosCmd.setScfold(encode(p))
  }

  implicit private val encodeHrwfsPickupPosition: EncodeEpicsValue[HrwfsPickupPosition, String] = EncodeEpicsValue((op: HrwfsPickupPosition)
  => op match {
      case HrwfsPickupPosition.IN => "IN"
      case HrwfsPickupPosition.OUT => "OUT"
      case HrwfsPickupPosition.Parked => "park-pos."
    })

  def setHRPickupConfig(hrwfsPos: HrwfsPickupPosition): SeqAction[Unit] = hrwfsPos match {
    case HrwfsPickupPosition.Parked => TcsEpics().hrwfsParkCmd.mark
    case _ => TcsEpics().hrwfsPosCmd.setHrwfsPos(encode(hrwfsPos))
  }

  private def setAGConfig(c: AGConfig): SeqAction[Unit] = for {
    _ <- setScienceFoldConfig(c.sfPos)
    _ <- setHRPickupConfig(c.hrwfsPos)
  } yield TrySeq(())


  implicit private val encodeMountGuideConfig: EncodeEpicsValue[MountGuideOption, String] = EncodeEpicsValue((op: MountGuideOption)
  => op match {
      case MountGuideOn => "on"
      case MountGuideOff => "off"
    })

  private def setMountGuide(c: MountGuideOption): SeqAction[Unit] = TcsEpics().mountGuideCmd.setMode(encode(c))

  implicit private val encodeM1GuideConfig: EncodeEpicsValue[M1GuideConfig, String] = EncodeEpicsValue((op: M1GuideConfig)
  => op match {
      case M1GuideOn(_) => "on"
      case M1GuideOff => "off"
    })

  private def setM1Guide(c: M1GuideConfig): SeqAction[Unit] = TcsEpics().m1GuideCmd.setState(encode(c))

  implicit private val encodeM2GuideConfig: EncodeEpicsValue[M2GuideConfig, String] = EncodeEpicsValue((op: M2GuideConfig)
  => op match {
      case M2GuideOn(_, _) => "on"
      case M2GuideOff => "off"
    })

  private def setM2Guide(c: M2GuideConfig): SeqAction[Unit] = TcsEpics().m2GuideCmd.setState(encode(c))

  implicit private val decodeInPosition: DecodeEpicsValue[String, Boolean] = DecodeEpicsValue(x => x.trim == "TRUE")

  override def applyConfig(tc: TelescopeConfig, gtc: GuidersTrackingConfig, ge: GuidersEnabled, agc: AGConfig): SeqAction[Unit] =
    for {
      _ <- setTelescopeConfig(tc)
      _ <- setProbesTrackingConfig(gtc)
      _ <- setGuidersWfs(ge)
      _ <- setAGConfig(agc)
      _ <- TcsEpics().post
      _ <- EitherT(Task(Log.info("TCS configuration command post").right))
      _ <- TcsEpics().waitInPosition(Seconds(30))
      _ <- EitherT(Task(Log.info("TCS inposition").right))
    } yield TrySeq(())

  override def guide(gc: GuideConfig): SeqAction[Unit] = for {
    _ <- setMountGuide(gc.mountGuide)
    _ <- setM1Guide(gc.m1Guide)
    _ <- setM2Guide(gc.m2Guide)
    _ <- TcsEpics().post
  } yield TrySeq(())

}
