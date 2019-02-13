// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import seqexec.server.tcs.TcsController.{HrwfsConfig, _}
import seqexec.server.{EpicsCodex, EpicsCommand, SeqAction, SeqexecFailure, TrySeq}
import edu.gemini.spModel.core.Wavelength
import org.log4s.getLogger
import squants.space.{Angstroms, Degrees, Millimeters}
import squants.time.Seconds
import cats.data._
import cats.effect.IO
import cats.implicits._
import edu.gemini.seqexec.server.tcs.{BinaryOnOff, BinaryYesNo}
import gem.enum.LightSinkName

object TcsControllerEpics extends TcsController {
  private val Log = getLogger

  import EpicsCodex._
  import FollowOption._
  import MountGuideOption._

  val BottomPort = 1

  // Code to retrieve the current configuration from TCS. Include a lot of decoders
  implicit private val decodeMountGuideOption: DecodeEpicsValue[Int, MountGuideOption] = DecodeEpicsValue((d: Int)
  => if (d === 0) MountGuideOff else MountGuideOn)

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

  private def decodeGuideSourceOption(s: String): Boolean = s.trim =!= "OFF"

  implicit private val decodeComaOption: DecodeEpicsValue[String, ComaOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") ComaOption.ComaOff else ComaOption.ComaOn)

  private def decodeM2Guide(s: BinaryOnOff, u: ComaOption, v: Set[TipTiltSource]): M2GuideConfig =
    if (s === BinaryOnOff.Off) M2GuideOff
    else M2GuideOn(u, v)

  private def getGuideConfig: IO[TrySeq[GuideConfig]] = {
    for {
      mountGuide <-  OptionT(TcsEpics.instance.absorbTipTilt).map(decode[Int, MountGuideOption])
      m1Source   <-  OptionT(TcsEpics.instance.m1GuideSource).map(decode[String, M1Source])
      m1Guide    <-  OptionT(TcsEpics.instance.m1Guide).map(decodeM1Guide(_, m1Source))
      m2p1Guide  <-  OptionT(TcsEpics.instance.m2p1Guide).map(decodeGuideSourceOption)
      m2p2Guide  <-  OptionT(TcsEpics.instance.m2p2Guide).map(decodeGuideSourceOption)
      m2oiGuide  <-  OptionT(TcsEpics.instance.m2oiGuide).map(decodeGuideSourceOption)
      m2aoGuide  <-  OptionT(TcsEpics.instance.m2aoGuide).map(decodeGuideSourceOption)
      m2Coma     <-  OptionT(TcsEpics.instance.comaCorrect).map(decode[String, ComaOption])
      m2Guide    <- OptionT(Nested(TcsEpics.instance.m2GuideState).map(decodeM2Guide(_, m2Coma, List((m2p1Guide, TipTiltSource.PWFS1),
        (m2p2Guide, TipTiltSource.PWFS2), (m2oiGuide, TipTiltSource.OIWFS),
        (m2aoGuide, TipTiltSource.GAOS)).foldLeft(Set.empty[TipTiltSource])((s: Set[TipTiltSource], v: (Boolean, TipTiltSource)) => if (v._1) s + v._2 else s))).value)
    } yield TrySeq(GuideConfig(mountGuide, m1Guide, m2Guide))
  }.value.map(_.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read guide configuration from TCS."))))

  implicit private val decodeBeam: DecodeEpicsValue[String, Beam] = DecodeEpicsValue{
    case "A" => Beam.A
    case "B" => Beam.B
    case "C" => Beam.C
    case _   => Beam.A
  }

  private def getTelescopeConfig: IO[TrySeq[TelescopeConfig]] = {
    for {
      xOffsetA    <- OptionT(TcsEpics.instance.xoffsetPoA1)
      yOffsetA    <- OptionT(TcsEpics.instance.yoffsetPoA1)
      xOffsetB    <- OptionT(TcsEpics.instance.xoffsetPoB1)
      yOffsetB    <- OptionT(TcsEpics.instance.yoffsetPoB1)
      xOffsetC    <- OptionT(TcsEpics.instance.xoffsetPoC1)
      yOffsetC    <- OptionT(TcsEpics.instance.yoffsetPoC1)
      wavelengthA <- OptionT(TcsEpics.instance.sourceAWavelength)
      wavelengthB <- OptionT(TcsEpics.instance.sourceBWavelength)
      wavelengthC <- OptionT(TcsEpics.instance.sourceCWavelength)
      m2Beam      <-  OptionT(TcsEpics.instance.chopBeam).map(decode[String, Beam])
    } yield TrySeq(TelescopeConfig(
      OffsetA(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetA)), OffsetY(Millimeters[Double](yOffsetA)))),
      OffsetB(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetB)), OffsetY(Millimeters[Double](yOffsetB)))),
      OffsetC(FocalPlaneOffset(OffsetX(Millimeters[Double](xOffsetC)), OffsetY(Millimeters[Double](yOffsetC)))),
      WavelengthA(Wavelength(Angstroms[Double](wavelengthA))),
      WavelengthB(Wavelength(Angstroms[Double](wavelengthB))),
      WavelengthC(Wavelength(Angstroms[Double](wavelengthC))),
      m2Beam
    ))
  }.value.map(_.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read telescope configuration from TCS."))))

  private def decodeNodChopOption(s: String): Boolean = s.trim === "On"

  private def getNodChopTrackingConfig(g: TcsEpics.ProbeGuideConfig[IO]): IO[Option[NodChopTrackingConfig]] = (
    for {
      aa <-  OptionT(g.nodachopa).map(decodeNodChopOption)
      ab <-  OptionT(g.nodachopb).map(decodeNodChopOption)
      ac <-  OptionT(g.nodachopc).map(decodeNodChopOption)
      ba <-  OptionT(g.nodbchopa).map(decodeNodChopOption)
      bb <-  OptionT(g.nodbchopb).map(decodeNodChopOption)
      bc <-  OptionT(g.nodbchopc).map(decodeNodChopOption)
      ca <-  OptionT(g.nodcchopa).map(decodeNodChopOption)
      cb <-  OptionT(g.nodcchopb).map(decodeNodChopOption)
      cc <-  OptionT(g.nodcchopc).map(decodeNodChopOption)

      // This last product is slightly tricky.
      o  <- OptionT(IO{
        if (List(aa, ab, ac, ba, bb, bc, ca, cb, cc).contains(true)) {
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
                  case h :: t => Some(NodChopTrackingConfig.Special(OneAnd(h, t)))
                  case Nil    => None // the list is empty
                }
              }
            } else Some(NodChopTrackingConfig.None)
      })
    } yield o
  ).value

  private def calcProbeTrackingConfig(f: FollowOption, t: NodChopTrackingConfig): ProbeTrackingConfig = (f, t) match {
    case (_, NodChopTrackingConfig.None)              => ProbeTrackingConfig.Off
    case (FollowOn, NodChopTrackingConfig.Normal)     => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
    case (FollowOn, v: NodChopTrackingConfig.Special) => ProbeTrackingConfig.On(v)
    case _                                            => ProbeTrackingConfig.Off
  }

  implicit private val decodeFollowOption: DecodeEpicsValue[String, FollowOption] = DecodeEpicsValue((s: String)
  => if (s.trim === "Off") FollowOff else FollowOn)

  private def getGuidersTrackingConfig: IO[TrySeq[GuidersTrackingConfig]] = {
    for {
      p1       <- OptionT(getNodChopTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideConfig))
      p2       <- OptionT(getNodChopTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideConfig))
      oi       <- OptionT(getNodChopTrackingConfig(TcsEpics.instance.oiwfsProbeGuideConfig))
      p1Follow <-  OptionT(TcsEpics.instance.p1FollowS).map(decode[String, FollowOption])
      p2Follow <-  OptionT(TcsEpics.instance.p2FollowS).map(decode[String, FollowOption])
      oiFollow <-  OptionT(TcsEpics.instance.oiFollowS).map(decode[String, FollowOption])
    } yield TrySeq(GuidersTrackingConfig(ProbeTrackingConfigP1(calcProbeTrackingConfig(p1Follow, p1)),
      ProbeTrackingConfigP2(calcProbeTrackingConfig(p2Follow, p2)),
      ProbeTrackingConfigOI(calcProbeTrackingConfig(oiFollow, oi)),
      ProbeTrackingConfigAO(ProbeTrackingConfig.Off)))
  }.value.map(_.getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read probes guide from TCS."))))

  implicit private val decodeGuideSensorOption: DecodeEpicsValue[BinaryYesNo, GuiderSensorOption] =
    DecodeEpicsValue((s: BinaryYesNo) => if (s === BinaryYesNo.No) GuiderSensorOff else GuiderSensorOn)

  private def getGuidersEnabled: IO[TrySeq[GuidersEnabled]] = {
    for {
      p1On <-  OptionT(TcsEpics.instance.pwfs1On).map(decode[BinaryYesNo, GuiderSensorOption])
      p2On <-  OptionT(TcsEpics.instance.pwfs2On).map(decode[BinaryYesNo, GuiderSensorOption])
      oiOn <-  OptionT(TcsEpics.instance.oiwfsOn).map(decode[BinaryYesNo, GuiderSensorOption])
    } yield TrySeq(GuidersEnabled(GuiderSensorOptionP1(p1On), GuiderSensorOptionP2(p2On),
        GuiderSensorOptionOI(oiOn)))
  }.value.map(_.getOrElse(TrySeq.fail(
    SeqexecFailure.Unexpected("Unable to read guider detectors state from TCS.")
  )))

  // Decoding and encoding the science fold position require some common definitions, therefore I
  // put them inside an object
  private[server] object CodexScienceFoldPosition {

    import LightSource._
    import ScienceFoldPosition._

    private val AO_PREFIX = "ao2"
    private val GCAL_PREFIX = "gcal2"
    private val PARK_POS = "park-pos"

    def portFromSinkName(n: LightSinkName): IO[Option[Int]] = {
      val InvalidPort = 0
      (n match {
        case LightSinkName.Gmos |
             LightSinkName.Gmos_Ifu => TcsEpics.instance.gmosPort
        case LightSinkName.Niri_f6 |
             LightSinkName.Niri_f14 |
             LightSinkName.Niri_f32 => TcsEpics.instance.niriPort
        case LightSinkName.Nifs => TcsEpics.instance.nifsPort
        case LightSinkName.Gnirs => TcsEpics.instance.gnirsPort
        case LightSinkName.F2 => TcsEpics.instance.f2Port
        case LightSinkName.Gpi => TcsEpics.instance.gpiPort
        case LightSinkName.Ghost => TcsEpics.instance.ghostPort
        case LightSinkName.Gsaoi => TcsEpics.instance.gsaoiPort
        case LightSinkName.Ac |
             LightSinkName.Hr => IO(BottomPort.some)
        case _ => IO(None)
      }).map(_.filterNot(_ === InvalidPort))
    }

    private def findSinkInSFName(str: String): Option[LightSinkName] =
      LightSinkName.all.find(i => str.startsWith(i.name))

    implicit val decodeScienceFoldPosition: DecodeEpicsValue[String, Option[ScienceFoldPosition]] = DecodeEpicsValue(
      (t: String) => if (t.startsWith(PARK_POS)) Parked.some
      else if (t.startsWith(AO_PREFIX))
        findSinkInSFName(t.substring(AO_PREFIX.length)).map(Position(AO, _))
      else if (t.startsWith(GCAL_PREFIX))
        findSinkInSFName(t.substring(GCAL_PREFIX.length)).map(Position(GCAL, _))
      else findSinkInSFName(t).map(Position(Sky, _))
    )

    def encodeScienceFoldPosition(port: Int): EncodeEpicsValue[Position, String] = EncodeEpicsValue((a: Position) => {
      val instAGName = a.sink.name + port.toString

      a.source match {
        case Sky => instAGName
        case AO => AO_PREFIX + instAGName
        case GCAL => GCAL_PREFIX + instAGName
      }
    })
  }

  import CodexScienceFoldPosition._

  private def getScienceFoldPosition: IO[Option[ScienceFoldPosition]] = (
    for {
      sfPosOpt <- OptionT(TcsEpics.instance.sfName).map(decode[String, Option[ScienceFoldPosition]])
      sfPos    <- OptionT(IO(sfPosOpt))
      sfParked <- OptionT(TcsEpics.instance.sfParked).map(_ =!= 0)
    } yield if (sfParked) ScienceFoldPosition.Parked
            else sfPos
  ).value

  implicit val decodeHwrsPickupPosition: DecodeEpicsValue[String, HrwfsPickupPosition] = DecodeEpicsValue((t: String)
  => if (t.trim === "IN") HrwfsPickupPosition.IN
    else HrwfsPickupPosition.OUT)

  private def getHrwfsPickupPosition: IO[Option[HrwfsPickupPosition]] = (
    for {
      hwPos <- OptionT(TcsEpics.instance.agHwName).map(decode[String, HrwfsPickupPosition])
      hwParked <- OptionT(TcsEpics.instance.agHwParked).map(_ =!= 0)
    } yield if (hwParked) HrwfsPickupPosition.Parked
      else hwPos
  ).value

  private def getAGConfig: IO[TrySeq[AGConfig]] =
    (getScienceFoldPosition, getHrwfsPickupPosition.map(_.map(HrwfsConfig.Manual))).mapN(AGConfig).map(TrySeq(_))

  private def getIAA: IO[TrySeq[InstrumentAlignAngle]] = TcsEpics.instance.instrAA.map(
    _.map(iaa => TrySeq(InstrumentAlignAngle(Degrees[Double](iaa))))
      .getOrElse(TrySeq.fail(SeqexecFailure.Unexpected("Unable to read IAA from TCS.")))
  )

  override def getConfig: SeqAction[TcsConfig] =
    for {
      gc <- EitherT(getGuideConfig)
      tc <- EitherT(getTelescopeConfig)
      gtc <- EitherT(getGuidersTrackingConfig)
      ge <- EitherT(getGuidersEnabled)
      agc <- EitherT(getAGConfig)
      iaa <- EitherT(getIAA)
    } yield TcsConfig(gc, tc, gtc, ge, agc, iaa)


  // Here starts the code that set the TCS configuration. There are a lot of encoders.
  implicit private val encodeBeam: EncodeEpicsValue[Beam, String] = EncodeEpicsValue{
    case Beam.A => "A"
    case Beam.B => "B"
    case Beam.C => "C"
  }

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
    EncodeEpicsValue {
      case NodChopTrackingOption.NodChopTrackingOn  => "on"
      case NodChopTrackingOption.NodChopTrackingOff => "off"
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

  private def setGuiderWfs(on: TcsEpics.WfsObserveCmd, off: EpicsCommand, c: GuiderSensorOption)
  : SeqAction[Unit] = {
    val NonStopExposures = -1
    c match {
      case GuiderSensorOff => off.mark
      case GuiderSensorOn => on.setNoexp(NonStopExposures) // Set number of exposures to non-stop (-1)
    }
  }

  // Special case: if source is the sky and the instrument is at the bottom port (port 1), the science fold must be parked.
  def setScienceFoldConfig(sfPos: ScienceFoldPosition): SeqAction[Unit] = sfPos match {
    case ScienceFoldPosition.Parked => TcsEpics.instance.scienceFoldParkCmd.mark
    case p@ScienceFoldPosition.Position(LightSource.Sky, sink) =>
      EitherT.liftF(portFromSinkName(sink)).flatMap(_.map(port =>
       if (port === BottomPort) TcsEpics.instance.scienceFoldParkCmd.mark
       else TcsEpics.instance.scienceFoldPosCmd.setScfold(encodeScienceFoldPosition(port).encode(p))
      ).getOrElse(SeqAction.void))
    case p: ScienceFoldPosition.Position =>
      EitherT.liftF(portFromSinkName(p.sink)).flatMap(_.map(port =>
        TcsEpics.instance.scienceFoldPosCmd.setScfold(encodeScienceFoldPosition(port).encode(p))
      ).getOrElse(SeqAction.void))
  }

  implicit private val encodeHrwfsPickupPosition: EncodeEpicsValue[HrwfsPickupPosition, String] =
    EncodeEpicsValue{
      case HrwfsPickupPosition.IN     => "IN"
      case HrwfsPickupPosition.OUT    => "OUT"
      case HrwfsPickupPosition.Parked => "park-pos."
    }

  def setHRPickupConfig(hrwfsPos: HrwfsPickupPosition): SeqAction[Unit] = hrwfsPos match {
    case HrwfsPickupPosition.Parked => TcsEpics.instance.hrwfsParkCmd.mark
    case _ => TcsEpics.instance.hrwfsPosCmd.setHrwfsPos(encode(hrwfsPos))
  }

  private def setAGUnit(c: AGConfig): SeqAction[Unit] = {
    val sf = c.sfPos.map(setScienceFoldConfig).getOrElse(SeqAction.void)
    val hr = SeqAction(c.hrwfs).flatMap(_.map {
      case HrwfsConfig.Manual(h) => setHRPickupConfig(h)
      case HrwfsConfig.Auto      => c.sfPos.map{
        case ScienceFoldPosition.Position(_, sink) => EitherT.liftF(portFromSinkName(sink))
          .flatMap(_.map(p => if(p === BottomPort) setHRPickupConfig(HrwfsPickupPosition.Parked)
                              else SeqAction.void
          ).getOrElse(SeqAction.void))
        case _                                     => SeqAction.void
      }.getOrElse(SeqAction.void)
    }.getOrElse(SeqAction.void))

    sf *> hr
  }

  implicit private val encodeMountGuideConfig: EncodeEpicsValue[MountGuideOption, String] =
    EncodeEpicsValue{
      case MountGuideOn  => "on"
      case MountGuideOff => "off"
    }

  private def setMountGuide(c: MountGuideOption): SeqAction[Unit] =
    TcsEpics.instance.mountGuideCmd.setMode(encode(c))

  implicit private val encodeM1GuideConfig: EncodeEpicsValue[M1GuideConfig, String] =
    EncodeEpicsValue{
      case M1GuideOn(_) => "on"
      case M1GuideOff   => "off"
    }

  private def setM1Guide(c: M1GuideConfig): SeqAction[Unit] = TcsEpics.instance.m1GuideCmd.setState(encode(c))

  implicit private val encodeM2GuideConfig: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue{
      case M2GuideOn(_, _) => "on"
      case M2GuideOff      => "off"
    }

  private def setM2Guide(c: M2GuideConfig): SeqAction[Unit] = TcsEpics.instance.m2GuideCmd.setState(encode(c))

  private val tcsTimeout = Seconds(60)
  private val agTimeout = Seconds(60)

  override def applyConfig(subsystems: NonEmptyList[Subsystem], tcs: TcsConfig): SeqAction[Unit] = {
    def configSubsystem(subsystem: Subsystem, tcs: TcsConfig): SeqAction[Unit] = subsystem match {
      case Subsystem.M1     => setM1Guide(tcs.gc.m1Guide)
      case Subsystem.M2     => setM2Guide(tcs.gc.m2Guide)
      case Subsystem.OIWFS  =>
        setProbeTrackingConfig(TcsEpics.instance.oiwfsProbeGuideCmd, tcs.gtc.oiwfs.self) *>
          setGuiderWfs(TcsEpics.instance.oiwfsObserveCmd, TcsEpics.instance.oiwfsStopObserveCmd, tcs.ge.oiwfs.self)
      case Subsystem.P1WFS  =>
        setProbeTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideCmd, tcs.gtc.pwfs1.self) *>
          setGuiderWfs(TcsEpics.instance.pwfs1ObserveCmd, TcsEpics.instance.pwfs1StopObserveCmd, tcs.ge.pwfs1.self)
      case Subsystem.P2WFS  =>
        setProbeTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideCmd, tcs.gtc.pwfs2.self) *>
          setGuiderWfs(TcsEpics.instance.pwfs2ObserveCmd, TcsEpics.instance.pwfs2StopObserveCmd, tcs.ge.pwfs2.self)
      case Subsystem.Mount  => setTelescopeConfig(tcs.tc)
      case Subsystem.AGUnit => setAGUnit(tcs.agc)
    }

    subsystems.tail.foldLeft(configSubsystem(subsystems.head, tcs))((b, a) => b *> configSubsystem(a, tcs)) *>
      TcsEpics.instance.post *>
      EitherT.right(IO.apply(Log.debug("TCS configuration command post"))) *>
      (if(subsystems.toList.contains(Subsystem.Mount))
        TcsEpics.instance.waitInPosition(tcsTimeout) *> EitherT.right(IO.apply(Log.info("TCS inposition")))
      else if(subsystems.toList.intersect(List(Subsystem.P1WFS, Subsystem.P2WFS, Subsystem.AGUnit)).nonEmpty)
        TcsEpics.instance.waitAGInPosition(agTimeout) *> EitherT.right(IO.apply(Log.debug("AG inposition")))
      else SeqAction.void)
  }

  override def guide(gc: GuideConfig): SeqAction[Unit] = for {
    _ <- setMountGuide(gc.mountGuide)
    _ <- setM1Guide(gc.m1Guide)
    _ <- setM2Guide(gc.m2Guide)
    _ <- TcsEpics.instance.post
    _ <- EitherT.right(IO.apply(Log.info("TCS guide command post")))
  } yield ()

  override def notifyObserveStart: SeqAction[Unit] = TcsEpics.instance.observe.mark *> TcsEpics.instance.post.void

  override def notifyObserveEnd: SeqAction[Unit] = TcsEpics.instance.endObserve.mark *> TcsEpics.instance.post.void
}
