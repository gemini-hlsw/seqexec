// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.tcs

import edu.gemini.seqexec.server.tcs.TcsController._
import edu.gemini.seqexec.model.Model
import edu.gemini.seqexec.server.{EpicsCodex, EpicsCommand, SeqAction, SeqexecFailure, TrySeq}
import edu.gemini.spModel.core.Wavelength
import org.log4s.getLogger
import squants.space.{Angstroms, Degrees, Millimeters}
import squants.time.Seconds

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task

/**
 * Created by jluhrs on 9/7/15.
 */
object TcsControllerEpics extends TcsController {
  private val Log = getLogger

  import EpicsCodex._
  import FollowOption._
  import MountGuideOption._

  // Code to retrieve the current configuration from TCS. Include a lot of decoders
  implicit private val decodeMountGuideOption: DecodeEpicsValue[Integer, MountGuideOption] = DecodeEpicsValue((d: Integer)
  => if (d.toInt === 0) MountGuideOff else MountGuideOn)

  implicit private val decodeM1GuideSource: DecodeEpicsValue[String, M1Source] = DecodeEpicsValue((s: String)
  => s.trim match {
      case "PWFS1" => M1Source.PWFS1
      case "PWFS2" => M1Source.PWFS2
      case "OIWFS" => M1Source.OIWFS
      case "GAOS"  => M1Source.GAOS
      case _       => M1Source.PWFS1
    })

  private def decodeM1Guide(r: BinaryOnOff, s: M1Source): M1GuideConfig =
    if (r === BinaryOnOff.Off) M1GuideOff
    else M1GuideOn(s)

  private def decodeGuideSourceOption(s: String): Boolean = s.trim =/= "OFF"

  implicit private val decodeComaOption: DecodeEpicsValue[String, ComaOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") ComaOption.ComaOff else ComaOption.ComaOn)

  private def decodeM2Guide(s: BinaryOnOff, u: ComaOption, v: Set[TipTiltSource]): M2GuideConfig =
    if (s === BinaryOnOff.Off) M2GuideOff
    else M2GuideOn(u, v)

  private def getGuideConfig: TrySeq[GuideConfig] = {
    for {
      mountGuide <- TcsEpics.instance.absorbTipTilt.map(decode[Integer, MountGuideOption])
      m1Source   <- TcsEpics.instance.m1GuideSource.map(decode[String, M1Source])
      m1Guide    <- TcsEpics.instance.m1Guide.map(decodeM1Guide(_, m1Source))
      m2p1Guide  <- TcsEpics.instance.m2p1Guide.map(decodeGuideSourceOption)
      m2p2Guide  <- TcsEpics.instance.m2p2Guide.map(decodeGuideSourceOption)
      m2oiGuide  <- TcsEpics.instance.m2oiGuide.map(decodeGuideSourceOption)
      m2aoGuide  <- TcsEpics.instance.m2aoGuide.map(decodeGuideSourceOption)
      m2Coma     <- TcsEpics.instance.comaCorrect.map(decode[String, ComaOption])
      m2Guide    <- TcsEpics.instance.m2GuideState.map(decodeM2Guide(_, m2Coma, List((m2p1Guide, TipTiltSource.PWFS1),
        (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
        (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set[TipTiltSource]())((s: Set[TipTiltSource], v: (Boolean, TipTiltSource)) => if (v._1) s + v._2 else s)))
    } yield TrySeq(GuideConfig(mountGuide, m1Guide, m2Guide))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guide configuration from TCS.")))

  implicit private val decodeBeam: DecodeEpicsValue[String, Beam] = DecodeEpicsValue((op: String)
  => op match {
      case "A" => Beam.A
      case "B" => Beam.B
      case "C" => Beam.C
      case _   => Beam.A
    }
  )

  private def getTelescopeConfig: TrySeq[TelescopeConfig] = {
    for {
      xOffsetA    <- TcsEpics.instance.xoffsetPoA1
      yOffsetA    <- TcsEpics.instance.yoffsetPoA1
      xOffsetB    <- TcsEpics.instance.xoffsetPoB1
      yOffsetB    <- TcsEpics.instance.yoffsetPoB1
      xOffsetC    <- TcsEpics.instance.xoffsetPoC1
      yOffsetC    <- TcsEpics.instance.yoffsetPoC1
      wavelengthA <- TcsEpics.instance.sourceAWavelength
      wavelengthB <- TcsEpics.instance.sourceBWavelength
      wavelengthC <- TcsEpics.instance.sourceCWavelength
      m2Beam      <- TcsEpics.instance.chopBeam.map(decode[String, Beam])
    } yield TrySeq(TelescopeConfig(
      OffsetA(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetA)), OffsetY(Millimeters[Double](yOffsetA)))),
      OffsetB(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetB)), OffsetY(Millimeters[Double](yOffsetB)))),
      OffsetC(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetC)), OffsetY(Millimeters[Double](yOffsetC)))),
      WavelengthA(Wavelength(Angstroms[Double](wavelengthA))),
      WavelengthB(Wavelength(Angstroms[Double](wavelengthB))),
      WavelengthC(Wavelength(Angstroms[Double](wavelengthC))),
      m2Beam
    ))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read telescope configuration from TCS.")))

  private def decodeNodChopOption(s: String): Boolean = s.trim === "On"

  private def getNodChopTrackingConfig(g: TcsEpics.ProbeGuideConfig): Option[NodChopTrackingConfig] =
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

      // This last production is slightly tricky.
      o  <- if (List(aa, ab, ac, ba, bb, bc, ca, cb, cc).contains(true)) {
              if (List(aa, bb, cc).forall(_ === true) && List(ab, ac, ba, bc, ca, cb).forall(_ === false)) {
                Some(NodChopTrackingConfig.Normal)
              } else {
                List(
                  (aa, NodChop(Beam.A, Beam.A)), (ab, NodChop(Beam.A, Beam.B)), (ac, NodChop(Beam.A, Beam.C)),
                  (ba, NodChop(Beam.B, Beam.A)), (bb, NodChop(Beam.B, Beam.B)), (bc, NodChop(Beam.B, Beam.C)),
                  (ca, NodChop(Beam.C, Beam.A)), (cb, NodChop(Beam.C, Beam.B)), (cc, NodChop(Beam.C, Beam.C))
                ) collect {
                  case (true, a) => a
                } match {
                  case h :: t => Some(NodChopTrackingConfig.Special(OneAnd(h, t.toSet)))
                  case Nil    => None // the list is empty
                }
              }
            } else Some(NodChopTrackingConfig.None)

    } yield o

  private def calcProbeTrackingConfig(f: FollowOption, t: NodChopTrackingConfig): ProbeTrackingConfig = (f, t) match {
    case (_, NodChopTrackingConfig.None)              => ProbeTrackingConfig.Off
    case (FollowOn, NodChopTrackingConfig.Normal)     => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
    case (FollowOn, v: NodChopTrackingConfig.Special) => ProbeTrackingConfig.On(v)
    case _                                            => ProbeTrackingConfig.Off
  }

  implicit private val decodeFollowOption: DecodeEpicsValue[String, FollowOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") FollowOff else FollowOn)

  private def getGuidersTrackingConfig: TrySeq[GuidersTrackingConfig] = {
    for {
      p1       <- getNodChopTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideConfig)
      p2       <- getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig)
      oi       <- getNodChopTrackingConfig(TcsEpics.instance.oiwfsProbeGuideConfig)
      p1Follow <- TcsEpics.instance.p1FollowS.map(decode[String, FollowOption])
      p2Follow <- TcsEpics.instance.p2FollowS.map(decode[String, FollowOption])
      oiFollow <- TcsEpics.instance.oiFollowS.map(decode[String, FollowOption])
    } yield TrySeq(GuidersTrackingConfig(ProbeTrackingConfigP1(calcProbeTrackingConfig(p1Follow, p1)),
      ProbeTrackingConfigP2(calcProbeTrackingConfig(p2Follow, p2)),
      ProbeTrackingConfigOI(calcProbeTrackingConfig(oiFollow, oi)),
      ProbeTrackingConfigAO(ProbeTrackingConfig.Off)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read probes guide from TCS.")))

  implicit private val decodeGuideSensorOption: DecodeEpicsValue[BinaryYesNo, GuiderSensorOption] =
    DecodeEpicsValue((s: BinaryYesNo) => if (s === BinaryYesNo.No) GuiderSensorOff else GuiderSensorOn)

  private def getGuidersEnabled: TrySeq[GuidersEnabled] = {
    for {
      p1On <- TcsEpics.instance.pwfs1On.map(decode[BinaryYesNo, GuiderSensorOption])
      p2On <- TcsEpics.instance.pwfs2On.map(decode[BinaryYesNo, GuiderSensorOption])
      oiOn <- TcsEpics.instance.oiwfsOn.map(decode[BinaryYesNo, GuiderSensorOption])
    } yield TrySeq(GuidersEnabled(GuiderSensorOptionP1(p1On), GuiderSensorOptionP2(p2On), GuiderSensorOptionOI(oiOn)))
  }.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guider detectors state from TCS.")))

  private def getInstPort(inst: Model.Instrument): Option[Int] = (inst match {
    case Model.Instrument.GmosS |
         Model.Instrument.GmosN  => TcsEpics.instance.gmosPort
    case Model.Instrument.GSAOI  => TcsEpics.instance.gsaoiPort
    case Model.Instrument.F2     => TcsEpics.instance.f2Port
    case Model.Instrument.GPI    => TcsEpics.instance.gpiPort
    case Model.Instrument.NIRI   => TcsEpics.instance.niriPort
    case Model.Instrument.GNIRS  => TcsEpics.instance.gnirsPort
    case Model.Instrument.NIFS   => TcsEpics.instance.nifsPort
    case _                       => None
  }).flatMap(p => if (p === 0) None else Some(p))

  // Decoding and encoding the science fold position require some common definitions, therefore I put them inside an
  // object
  private object CodexScienceFoldPosition {

    import LightSource._
    import ScienceFoldPosition._

    private val AO_PREFIX = "ao2"
    private val GCAL_PREFIX = "gcal2"
    private val PARK_POS = "park-pos"

    final case class SFInstName(self: String) extends AnyVal
    object SFInstName {
      implicit val EqualSFInstName: Equal[SFInstName] =
        Equal.equalA // natural equality here
    }

    val instNameMap: Map[Model.Instrument, SFInstName] = Map(
      Model.Instrument.GmosS -> SFInstName("gmos"),
      Model.Instrument.GmosN -> SFInstName("gmos"),
      Model.Instrument.GSAOI -> SFInstName("gsaoi"),
      Model.Instrument.GNIRS -> SFInstName("gnirs"),
      Model.Instrument.F2    -> SFInstName("f2"),
      Model.Instrument.GPI   -> SFInstName("gpi")
    )

    private def findInstrument(str: String): Option[Model.Instrument] = instNameMap.find{ case (_, n) => str.startsWith(n.self)}.map(_._1)

    implicit val decodeScienceFoldPosition: DecodeEpicsValue[String, Option[ScienceFoldPosition]] = DecodeEpicsValue(
      (t: String) => if (t.startsWith(PARK_POS)) Parked.some
        else if (t.startsWith(AO_PREFIX)) findInstrument(t.substring(AO_PREFIX.length)).map(Position(AO, _))
        else if (t.startsWith(GCAL_PREFIX)) findInstrument(t.substring(GCAL_PREFIX.length)).map(Position(GCAL, _))
        else findInstrument(t).map(Position(Sky, _))
    )

    implicit val encodeScienceFoldPosition: EncodeEpicsValue[Position, Option[String]] = EncodeEpicsValue((a: Position)
    => {
      val instAGName = for {
        name <- instNameMap.get(a.sink)
        port <- getInstPort(a.sink)
      } yield name.self +  port.toString

      instAGName.map(n => a.source match {
        case Sky  => n
        case AO   => AO_PREFIX + n
        case GCAL => GCAL_PREFIX + n
      })
    }
    )
  }

  import CodexScienceFoldPosition._

  private def getScienceFoldPosition: Option[ScienceFoldPosition] = for {
    sfPosOpt <- TcsEpics.instance.sfName.map(decode[String, Option[ScienceFoldPosition]])
    sfPos    <- sfPosOpt
    sfParked <- TcsEpics.instance.sfParked.map {
      _.toInt =/= 0
    }
  } yield if (sfParked) ScienceFoldPosition.Parked
          else sfPos

  implicit val decodeHwrsPickupPosition: DecodeEpicsValue[String, HrwfsPickupPosition] = DecodeEpicsValue((t: String)
  => if (t.trim === "IN") HrwfsPickupPosition.IN
    else HrwfsPickupPosition.OUT)

  private def getHrwfsPickupPosition: Option[HrwfsPickupPosition] = for {
    hwPos <- TcsEpics.instance.agHwName.map(decode[String, HrwfsPickupPosition])
    hwParked <- TcsEpics.instance.agHwParked.map {
      _.toInt =/= 0
    }
  } yield if (hwParked) HrwfsPickupPosition.Parked
    else hwPos

  private def getAGConfig: TrySeq[AGConfig] = TrySeq(AGConfig(getScienceFoldPosition, getHrwfsPickupPosition))

  private def getIAA: TrySeq[InstrumentAlignAngle] = {
    for {
      iaa <- TcsEpics.instance.instrAA
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
    _ <- TcsEpics.instance.offsetACmd.setX(c.offsetA.self.x.self.toMillimeters)
    _ <- TcsEpics.instance.offsetACmd.setY(c.offsetA.self.y.self.toMillimeters)
    _ <- TcsEpics.instance.offsetBCmd.setX(c.offsetB.self.x.self.toMillimeters)
    _ <- TcsEpics.instance.offsetBCmd.setY(c.offsetB.self.y.self.toMillimeters)
    _ <- TcsEpics.instance.offsetCCmd.setX(c.offsetC.self.x.self.toMillimeters)
    _ <- TcsEpics.instance.offsetCCmd.setY(c.offsetC.self.y.self.toMillimeters)
    _ <- TcsEpics.instance.wavelSourceA.setWavel(c.wavelA.self.toMicrons)
    _ <- TcsEpics.instance.wavelSourceA.setWavel(c.wavelB.self.toMicrons)
    _ <- TcsEpics.instance.wavelSourceA.setWavel(c.wavelB.self.toMicrons)
    _ <- TcsEpics.instance.m2Beam.setBeam(encode(c.m2beam))
  } yield ()

  implicit private val encodeNodChopOption: EncodeEpicsValue[NodChopTrackingOption, String] =
    EncodeEpicsValue { (op: NodChopTrackingOption) =>
      op match {
        case NodChopTrackingOption.NodChopTrackingOn  => "on"
        case NodChopTrackingOption.NodChopTrackingOff => "off"
      }
    }

  private def setProbeTrackingConfig(s: TcsEpics.ProbeGuideCmd, c: ProbeTrackingConfig): SeqAction[Unit] = for {
    _ <- s.setNodachopa(encode(c.getNodChop.get(NodChop(Beam.A, Beam.A))))
    _ <- s.setNodachopb(encode(c.getNodChop.get(NodChop(Beam.A, Beam.B))))
    _ <- s.setNodachopc(encode(c.getNodChop.get(NodChop(Beam.A, Beam.C))))
    _ <- s.setNodbchopa(encode(c.getNodChop.get(NodChop(Beam.B, Beam.A))))
    _ <- s.setNodbchopb(encode(c.getNodChop.get(NodChop(Beam.B, Beam.B))))
    _ <- s.setNodbchopc(encode(c.getNodChop.get(NodChop(Beam.B, Beam.C))))
    _ <- s.setNodcchopa(encode(c.getNodChop.get(NodChop(Beam.C, Beam.A))))
    _ <- s.setNodcchopb(encode(c.getNodChop.get(NodChop(Beam.C, Beam.B))))
    _ <- s.setNodcchopc(encode(c.getNodChop.get(NodChop(Beam.C, Beam.C))))
  } yield ()

  private def setGuiderWfs(on: TcsEpics.WfsObserveCmd, off: EpicsCommand, c: GuiderSensorOption): SeqAction[Unit] =
    c match {
      case GuiderSensorOff => off.mark
      case GuiderSensorOn => on.setNoexp(-1) // Set number of exposures to non-stop (-1)
    }

  // Special case: if source is the sky and the instrument is at the bottom port (port 1), the science fold must be parked.
  def setScienceFoldConfig(sfPos: ScienceFoldPosition): SeqAction[Unit] = sfPos match {
    case ScienceFoldPosition.Parked => TcsEpics.instance.scienceFoldParkCmd.mark
    case p@ScienceFoldPosition.Position(LightSource.Sky, inst) => getInstPort(inst).flatMap(port =>
      if (port === 1) TcsEpics.instance.scienceFoldParkCmd.mark.some
      else encode(p).map(TcsEpics.instance.scienceFoldPosCmd.setScfold)
    ).getOrElse(SeqAction(()))
    case p: ScienceFoldPosition.Position => encode(p).map(TcsEpics.instance.scienceFoldPosCmd.setScfold).getOrElse(SeqAction(()))
  }

  implicit private val encodeHrwfsPickupPosition: EncodeEpicsValue[HrwfsPickupPosition, String] = EncodeEpicsValue((op: HrwfsPickupPosition)
  => op match {
      case HrwfsPickupPosition.IN     => "IN"
      case HrwfsPickupPosition.OUT    => "OUT"
      case HrwfsPickupPosition.Parked => "park-pos."
    })

  def setHRPickupConfig(hrwfsPos: HrwfsPickupPosition): SeqAction[Unit] = hrwfsPos match {
    case HrwfsPickupPosition.Parked => TcsEpics.instance.hrwfsParkCmd.mark
    case _ => TcsEpics.instance.hrwfsPosCmd.setHrwfsPos(encode(hrwfsPos))
  }

  private def setScienceFoldPosition(p: Option[ScienceFoldPosition]): SeqAction[Unit] = p.map(setScienceFoldConfig).getOrElse(SeqAction(()))

  private def setHrProbePosition(p: Option[HrwfsPickupPosition]): SeqAction[Unit] = p.map(setHRPickupConfig).getOrElse(SeqAction(()))

  implicit private val encodeMountGuideConfig: EncodeEpicsValue[MountGuideOption, String] = EncodeEpicsValue((op: MountGuideOption)
  => op match {
      case MountGuideOn  => "on"
      case MountGuideOff => "off"
    })

  private def setMountGuide(c: MountGuideOption): SeqAction[Unit] = TcsEpics.instance.mountGuideCmd.setMode(encode(c))

  implicit private val encodeM1GuideConfig: EncodeEpicsValue[M1GuideConfig, String] = EncodeEpicsValue((op: M1GuideConfig)
  => op match {
      case M1GuideOn(_) => "on"
      case M1GuideOff   => "off"
    })

  private def setM1Guide(c: M1GuideConfig): SeqAction[Unit] = TcsEpics.instance.m1GuideCmd.setState(encode(c))

  implicit private val encodeM2GuideConfig: EncodeEpicsValue[M2GuideConfig, String] = EncodeEpicsValue((op: M2GuideConfig)
  => op match {
      case M2GuideOn(_, _) => "on"
      case M2GuideOff      => "off"
    })

  private def setM2Guide(c: M2GuideConfig): SeqAction[Unit] = TcsEpics.instance.m2GuideCmd.setState(encode(c))

  private val tcsTimeout = Seconds(60)
  private val agTimeout = Seconds(60)

  override def applyConfig(subsystems: NonEmptyList[Subsystem], tcs: TcsConfig): SeqAction[Unit] = {
    def configSubsystem(subsystem: Subsystem): SeqAction[Unit] = subsystem match {
      case Subsystem.M1          => setM1Guide(tcs.gc.m1Guide)
      case Subsystem.M2          => setM2Guide(tcs.gc.m2Guide)
      case Subsystem.OIWFS       =>
        setProbeTrackingConfig(TcsEpics.instance.oiwfsProbeGuideCmd, tcs.gtc.oiwfs.self) *>
          setGuiderWfs(TcsEpics.instance.oiwfsObserveCmd, TcsEpics.instance.oiwfsStopObserveCmd, tcs.ge.oiwfs.self)
      case Subsystem.P1WFS       =>
        setProbeTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideCmd, tcs.gtc.pwfs1.self) *>
          setGuiderWfs(TcsEpics.instance.pwfs1ObserveCmd, TcsEpics.instance.pwfs1StopObserveCmd, tcs.ge.pwfs1.self)
      case Subsystem.P2WFS       =>
        setProbeTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideCmd, tcs.gtc.pwfs2.self) *>
          setGuiderWfs(TcsEpics.instance.pwfs2ObserveCmd, TcsEpics.instance.pwfs2StopObserveCmd, tcs.ge.pwfs2.self)
      case Subsystem.Mount       => setTelescopeConfig(tcs.tc)
      case Subsystem.HRProbe     => setHrProbePosition(tcs.agc.hrwfsPos)
      case Subsystem.ScienceFold => setScienceFoldPosition(tcs.agc.sfPos)
    }

    subsystems.tail.foldLeft(configSubsystem(subsystems.head))((b, a) => b *> configSubsystem(a)) *>
      TcsEpics.instance.post *>
      EitherT(Task(Log.info("TCS configuration command post").right)) *>
      (if(subsystems.toList.contains(Subsystem.Mount))
        TcsEpics.instance.waitInPosition(tcsTimeout) *> EitherT(Task(Log.info("TCS inposition").right))
      else if(subsystems.toList.contains(Subsystem.ScienceFold))
        TcsEpics.instance.waitAGInPosition(agTimeout) *> EitherT(Task(Log.info("AG inposition").right))
      else SeqAction.void)
  }

  override def guide(gc: GuideConfig): SeqAction[Unit] = for {
    _ <- setMountGuide(gc.mountGuide)
    _ <- setM1Guide(gc.m1Guide)
    _ <- setM2Guide(gc.m2Guide)
    _ <- TcsEpics.instance.post
    _ <- EitherT(Task(Log.info("TCS guide command post").right))
  } yield ()

  override def notifyObserveStart: SeqAction[Unit] = TcsEpics.instance.observe.mark *> TcsEpics.instance.post.map(_ => ())

  override def notifyObserveEnd: SeqAction[Unit] = TcsEpics.instance.endObserve.mark *> TcsEpics.instance.post.map(_ => ())
}
