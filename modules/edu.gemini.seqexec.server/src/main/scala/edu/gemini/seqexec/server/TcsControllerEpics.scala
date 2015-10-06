package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.TcsController._
import edu.gemini.epics.acm.{CaService, XMLBuilder}
import edu.gemini.spModel.core.Wavelength
import squants.space.{Degrees, Microns, Millimeters}

import scalaz.concurrent.Task

/**
 * Created by jluhrs on 9/7/15.
 */
object TcsControllerEpics extends TcsController {

  val CA_CONFIG_FILE = "resources/Tcs.xml"
  val TCS_STATUS_ACCEPTOR = "tcsstate"

  (new XMLBuilder).fromFile(CA_CONFIG_FILE).buildAll()

  val tcsState = CaService.getInstance().getStatusAcceptor(TCS_STATUS_ACCEPTOR)

  private def getGuideConfig: TrySeq[GuideConfig] = {
    for {
      mountGuide <- Option(tcsState.getStringAttribute("absorbTipTilt").value).map { x => if (x == "Off") MountGuideOff else MountGuideOn }
      m1Source   <- Option(tcsState.getStringAttribute("m1GuideSource").value).map {
                    case "PWFS1" => M1Source.PWFS1
                    case "PWFS2" => M1Source.PWFS2
                    case "OIWFS" => M1Source.OIWFS
                    case "GAOS"  => M1Source.GAOS
                    case _       => M1Source.PWFS1
                  }
      m1Guide    <- Option(tcsState.getStringAttribute("m1Guide").value).map { x => if (x == "Off") M1GuideOff else M1GuideOn(m1Source) }
      m2p1Guide  <- Option(tcsState.getStringAttribute("m2p1Guide").value).map { x => x.trim == "OFF" }
      m2p2Guide  <- Option(tcsState.getStringAttribute("m2p2Guide").value).map { x => x.trim == "OFF" }
      m2oiGuide  <- Option(tcsState.getStringAttribute("m2oiGuide").value).map { x => x.trim == "OFF" }
      m2aoGuide  <- Option(tcsState.getStringAttribute("m2aoGuide").value).map { x => x.trim == "OFF" }
      m2Coma     <- Option(tcsState.getStringAttribute("comaCorrect").value).map { x => if (x == "Off") ComaOff else ComaOn }
      m2Guide    <- Option(tcsState.getStringAttribute("m2GuideState").value).map {
        x => if (x == "Off") M2GuideOff else M2GuideOn(m2Coma,
          List((m2p1Guide, TipTiltSource.PWFS1), (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
            (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set[TipTiltSource]())(
              (s:Set[TipTiltSource], v:(Boolean, TipTiltSource)) => if(v._1) s+v._2 else s ) ) }
    } yield TrySeq(GuideConfig(mountGuide, m1Guide, m2Guide))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guide configuration from TCS.")))

  private def getTelescopeConfig: TrySeq[TelescopeConfig] = {
    for {
      xOffsetA <- Option(tcsState.getDoubleAttribute("xoffsetPoA1").value)
      yOffsetA <- Option(tcsState.getDoubleAttribute("yoffsetPoA1").value)
      xOffsetB <- Option(tcsState.getDoubleAttribute("xoffsetPoB1").value)
      yOffsetB <- Option(tcsState.getDoubleAttribute("yoffsetPoB1").value)
      xOffsetC <- Option(tcsState.getDoubleAttribute("xoffsetPoC1").value)
      yOffsetC <- Option(tcsState.getDoubleAttribute("yoffsetPoC1").value)
      wavelengthA <- Option(tcsState.getDoubleAttribute("sourceAWavelength").value)
      wavelengthB <- Option(tcsState.getDoubleAttribute("sourceBWavelength").value)
      wavelengthC <- Option(tcsState.getDoubleAttribute("sourceCWavelength").value)
      m2Beam <- Option(tcsState.getStringAttribute("chopBeam").value).map {
                case "A" => Beam.A
                case "B" => Beam.B
                case "C" => Beam.C
                case _ => Beam.A
              }
    } yield TrySeq(TelescopeConfig(
        OffsetA(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetA)), OffsetY(Millimeters[Double](yOffsetA)))),
        OffsetB(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetB)), OffsetY(Millimeters[Double](yOffsetB)))),
        OffsetC(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetC)), OffsetY(Millimeters[Double](yOffsetC)))),
        WavelengthA(Wavelength(Microns[Double](wavelengthA))),
        WavelengthB(Wavelength(Microns[Double](wavelengthB))),
        WavelengthC(Wavelength(Microns[Double](wavelengthC))),
        m2Beam
    ))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guide telescope from TCS.")))

  private def getNodChopTrackingConfig(prefix: String): Option[NodChopTrackingConfig] = {
    for {
      aa <- Option(tcsState.getStringAttribute(prefix+"nodachopa").value).map { _ == "On"}
      ab <- Option(tcsState.getStringAttribute(prefix+"nodachopb").value).map { _ == "On"}
      ac <- Option(tcsState.getStringAttribute(prefix+"nodbahopc").value).map { _ == "On"}
      ba <- Option(tcsState.getStringAttribute(prefix+"nodbchopa").value).map { _ == "On"}
      bb <- Option(tcsState.getStringAttribute(prefix+"nodbchopb").value).map { _ == "On"}
      bc <- Option(tcsState.getStringAttribute(prefix+"nodbchopc").value).map { _ == "On"}
      ca <- Option(tcsState.getStringAttribute(prefix+"nodcchopa").value).map { _ == "On"}
      cb <- Option(tcsState.getStringAttribute(prefix+"nodcchopb").value).map { _ == "On"}
      cc <- Option(tcsState.getStringAttribute(prefix+"nodcchopc").value).map { _ == "On"}
    } yield {
      if (List(aa, ab, ac, ba, bb, bc, ca, cb, cc).exists(x=>x)) {
        if ( List(aa, bb, cc).forall(x=>x) && !List(ab, ac, ba, bc, ca, cb).exists(x=>x))
          NodChopTrackingConfig.Normal
        else NodChopTrackingConfig.Special(List((aa, NodChop(Beam.A, Beam.A)), (ab, NodChop(Beam.A, Beam.B)),
          (ac, NodChop(Beam.A, Beam.C)), (ba, NodChop(Beam.B, Beam.A)), (bb, NodChop(Beam.B, Beam.B)),
          (bc, NodChop(Beam.B, Beam.C)), (ca, NodChop(Beam.C, Beam.A)), (cb, NodChop(Beam.C, Beam.B)),
          (cc, NodChop(Beam.C, Beam.C))
        ).foldLeft(Set[NodChop]())((s:Set[NodChop], v:(Boolean, NodChop)) => if(v._1) s+v._2 else s ))
      }
      else NodChopTrackingConfig.None
    }
  }

  private def calcProbeTrackingConfig(f: FollowOption, t: NodChopTrackingConfig): ProbeTrackingConfig = (f, t) match {
    case (_, NodChopTrackingConfig.None)              => ProbeTrackingConfig.Off
    case (FollowOn, NodChopTrackingConfig.Normal)     => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
    case (FollowOn, v: NodChopTrackingConfig.Special) => ProbeTrackingConfig.On(v)
    case _                                            => ProbeTrackingConfig.Off
  }

  private def getGuidersTrackingConfig: TrySeq[GuidersTrackingConfig] = {
    for {
      p1 <- getNodChopTrackingConfig("p1")
      p2 <- getNodChopTrackingConfig("p2")
      oi <- getNodChopTrackingConfig("oi")
      p1Follow <- Option(tcsState.getStringAttribute("p1FollowS").value).map { x => if (x == "Off") FollowOff else FollowOn}
      p2Follow <- Option(tcsState.getStringAttribute("p2FollowS").value).map { x => if (x == "Off") FollowOff else FollowOn}
      oiFollow <- Option(tcsState.getStringAttribute("oiFollowS").value).map { x => if (x == "Off") FollowOff else FollowOn}
    } yield TrySeq(GuidersTrackingConfig(ProbeTrackingConfigP1(calcProbeTrackingConfig(p1Follow, p1)),
      ProbeTrackingConfigP2(calcProbeTrackingConfig(p2Follow, p2)),
      ProbeTrackingConfigOI(calcProbeTrackingConfig(oiFollow, oi)),
      ProbeTrackingConfigAO(ProbeTrackingConfig.Off)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read probes guide from TCS.")))

  private def getGuidersEnabled: TrySeq[GuidersEnabled] = {
    for {
      p1On <- Option(tcsState.getStringAttribute("pwfs1On").value).map { x => if (x == "Off") GuiderSensorOff else GuiderSensorOn}
      p2On <- Option(tcsState.getStringAttribute("pwfs2On").value).map { x => if (x == "Off") GuiderSensorOff else GuiderSensorOn}
      oiOn <- Option(tcsState.getStringAttribute("oiwfsOn").value).map { x => if (x == "Off") GuiderSensorOff else GuiderSensorOn}
      aoOn <- Option(tcsState.getStringAttribute("aowfsOn").value).map { x => if (x == "Off") GuiderSensorOff else GuiderSensorOn}
    } yield TrySeq(GuidersEnabled(GuiderSensorOptionP1(p1On), GuiderSensorOptionP2(p2On), GuiderSensorOptionOI(oiOn),
        GuiderSensorOptionAO(aoOn)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guider detectors state from TCS.")))

  private def getScienceFoldPosition: Option[ScienceFoldPosition] = for {
    sfPos    <- Option(tcsState.getStringAttribute("sfName").value)
    sfParked <- Option(tcsState.getIntegerAttribute("sfParked").value).map {_ != 0}
  } yield if (sfParked) ScienceFoldPosition.Parked
          else ScienceFoldPosition.fromPositionName(sfPos)

  private def getHrwfsPickupPosition: Option[HrwfsPickupPosition] = for {
    hwPos    <- Option(tcsState.getStringAttribute("agHwName").value)
    hwParked <- Option(tcsState.getIntegerAttribute("agHwParked").value).map {_ != 0}
  } yield if (hwParked) HrwfsPickupPosition.Parked
          else if(hwPos == "IN") HrwfsPickupPosition.IN
          else HrwfsPickupPosition.OUT

  private def getAGConfig: TrySeq[AGConfig] = {
    for {
      sf <- getScienceFoldPosition
      hrwfs <- getHrwfsPickupPosition
    } yield TrySeq(AGConfig(sf, hrwfs))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read AG state from TCS.")))

  private def getIAA: TrySeq[InstrumentAlignAngle] = {
    for {
      iaa <- Option(tcsState.getDoubleAttribute("instrAA").value)
    } yield TrySeq(InstrumentAlignAngle(Degrees[Double](iaa)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read IAA from TCS.")))

  override def getConfig: SeqAction[TcsConfig] = Task {
    for {
      gc  <- getGuideConfig
      tc  <- getTelescopeConfig
      gtc <- getGuidersTrackingConfig
      ge  <- getGuidersEnabled
      agc <- getAGConfig
      iaa <- getIAA
    } yield TcsConfig(gc, tc, gtc, ge, agc, iaa)
  }

  private def setTelescopeConfig(c: TelescopeConfig): SeqAction[Unit] = ???

  override def applyConfig(tc: TelescopeConfig, gtc: GuidersTrackingConfig, ge: GuidersEnabled, agc: AGConfig): SeqAction[Unit] = ???

  private def setMountGuide(c: MountGuideOption): SeqAction[Unit] = {
    if(c.active) TcsEpics.mountGuide.setMode("on")
    else TcsEpics.mountGuide.setMode("off")
  }
  private def setM1Guide(c: M1GuideConfig): SeqAction[Unit] = {
    if(c.active) TcsEpics.m1Guide.setState("on")
    else TcsEpics.m1Guide.setState("off")
  }

  private def setM2Guide(c: M2GuideConfig): SeqAction[Unit] = {
    if(c.active) TcsEpics.m2Guide.setState("on")
    else TcsEpics.m2Guide.setState("off")
  }

  override def guide(gc: GuideConfig): SeqAction[Unit] = {
    for {
      _ <- setMountGuide(gc.mountGuide)
      _ <- setM1Guide(gc.m1Guide)
      _ <- setM2Guide(gc.m2Guide)
    } yield TrySeq(())
  }

}
