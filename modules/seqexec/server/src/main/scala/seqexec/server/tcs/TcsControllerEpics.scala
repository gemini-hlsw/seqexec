// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.{Endo, Eq}
import seqexec.server.tcs.TcsController.{HrwfsConfig, TelescopeGuideConfig, _}
import seqexec.server.{EpicsCodex, EpicsCommand, SeqAction, SeqActionF, SeqexecFailure}
import edu.gemini.spModel.core.Wavelength
import org.log4s.getLogger
import squants.time.Seconds
import cats.data._
import cats.effect.IO
import cats.implicits._
import gem.enum.LightSinkName
import mouse.boolean._
import seqexec.server.altair.Altair
import seqexec.server.gems.Gems
import squants.Angle
import monocle.{Iso, Lens}
import monocle.macros.Lenses
import seqexec.server.tcs.Gaos.{GaosStarOff, GaosStarOn, OffsetMove, OffsetReached, OiOff, OiOn, P1Off, P1On, PauseCondition, PauseResume, ResumeCondition}
import seqexec.server.tcs.TcsController.FollowOption.FollowOn
import seqexec.server.tcs.TcsEpics.{ProbeFollowCmd, ProbeGuideCmd}
import shapeless.tag
import shapeless.tag.@@

object TcsControllerEpics extends TcsController {
  private val Log = getLogger

  import EpicsCodex._
  import MountGuideOption._
  import ScienceFoldPositionCodex._

  // Same offset is applied to all the beams
  private def setTelescopeOffset(c: FocalPlaneOffset): SeqAction[Unit] =
    TcsEpics.instance.offsetACmd.setX(c.x.toMillimeters) *>
      TcsEpics.instance.offsetACmd.setY(c.y.toMillimeters) *>
      TcsEpics.instance.offsetBCmd.setX(c.x.toMillimeters) *>
      TcsEpics.instance.offsetBCmd.setY(c.y.toMillimeters) *>
      TcsEpics.instance.offsetCCmd.setX(c.x.toMillimeters) *>
      TcsEpics.instance.offsetCCmd.setY(c.y.toMillimeters)

  // Same wavelength is applied to all the beams
  private def setWavelength(w: Wavelength): SeqAction[Unit] =
    TcsEpics.instance.wavelSourceA.setWavel(w.toMicrons) *>
      TcsEpics.instance.wavelSourceB.setWavel(w.toMicrons) *>
      TcsEpics.instance.wavelSourceC.setWavel(w.toMicrons)

  implicit private val encodeNodChopOption: EncodeEpicsValue[NodChopTrackingOption, String] =
    EncodeEpicsValue {
      case NodChopTrackingOption.NodChopTrackingOn  => "On"
      case NodChopTrackingOption.NodChopTrackingOff => "Off"
    }

  implicit private val encodeFollowOption: EncodeEpicsValue[FollowOption, String] =
    EncodeEpicsValue {
      case FollowOption.FollowOn  => "On"
      case FollowOption.FollowOff => "Off"
    }

  private def setNodChopProbeTrackingConfig(s: TcsEpics.ProbeGuideCmd)(c: NodChopTrackingConfig): SeqAction[Unit] = for {
    _ <- s.setNodachopa(encode(c.get(NodChop(Beam.A, Beam.A))))
    _ <- s.setNodachopb(encode(c.get(NodChop(Beam.A, Beam.B))))
    _ <- s.setNodachopc(encode(c.get(NodChop(Beam.A, Beam.C))))
    _ <- s.setNodbchopa(encode(c.get(NodChop(Beam.B, Beam.A))))
    _ <- s.setNodbchopb(encode(c.get(NodChop(Beam.B, Beam.B))))
    _ <- s.setNodbchopc(encode(c.get(NodChop(Beam.B, Beam.C))))
    _ <- s.setNodcchopa(encode(c.get(NodChop(Beam.C, Beam.A))))
    _ <- s.setNodcchopb(encode(c.get(NodChop(Beam.C, Beam.B))))
    _ <- s.setNodcchopc(encode(c.get(NodChop(Beam.C, Beam.C))))
  } yield ()

  private def setGuideProbe(guideControl: GuideControl, trkSet: ProbeTrackingConfig => EpicsTcsConfig => EpicsTcsConfig)
                           (subsystems: NonEmptySet[Subsystem], c: ProbeTrackingConfig, d: ProbeTrackingConfig)
  : Option[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] =
    if(subsystems.contains(guideControl.subs)) {
      val actions = List(
        (c.getNodChop =!= d.getNodChop)
          .option(setNodChopProbeTrackingConfig(guideControl.nodChopGuideCmd)(d.getNodChop)),
        d match {
          case ProbeTrackingConfig.Parked => (c =!= ProbeTrackingConfig.Parked).option(guideControl.parkCmd.mark)
          case ProbeTrackingConfig.On(_) |
               ProbeTrackingConfig.Off   |
               ProbeTrackingConfig.Frozen => (c.follow =!= d.follow)
            .option(guideControl.followCmd.setFollowState(encode(d.follow)))
        }
      ).collect{ case Some(x) => x }

      actions.nonEmpty.option{ x => actions.sequence *>
        SeqAction(trkSet(d)(x))
      }
    }
    else none

  private val pwfs1GuiderControl: GuideControl = GuideControl(Subsystem.PWFS1, TcsEpics.instance.pwfs1Park,
    TcsEpics.instance.pwfs1ProbeGuideCmd, TcsEpics.instance.pwfs1ProbeFollowCmd)

  private val setPwfs1Probe = setGuideProbe(pwfs1GuiderControl, (EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.tracking).set)(
    _, _, _)

  private val pwfs2GuiderControl: GuideControl = GuideControl(Subsystem.PWFS2, TcsEpics.instance.pwfs2Park,
    TcsEpics.instance.pwfs2ProbeGuideCmd, TcsEpics.instance.pwfs2ProbeFollowCmd)

  private val setPwfs2Probe = setGuideProbe(pwfs2GuiderControl,
    v => EpicsTcsConfig.pwfs2OrAowfs.modify(_.leftMap(GuiderConfig.tracking.set(v))))(_, _, _)

  private val oiwfsGuiderControl: GuideControl = GuideControl(Subsystem.OIWFS, TcsEpics.instance.oiwfsPark,
    TcsEpics.instance.oiwfsProbeGuideCmd, TcsEpics.instance.oiwfsProbeFollowCmd)

  private val setOiwfsProbe = setGuideProbe(oiwfsGuiderControl, (EpicsTcsConfig.oiwfs ^|-> GuiderConfig.tracking).set)(
    _, _, _)

  private def setAltairProbe(subsystems: NonEmptySet[Subsystem], c: ProbeTrackingConfig, d: ProbeTrackingConfig)
  : Option[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] =
    if(subsystems.contains(Subsystem.Gaos)) {
      val actions = List(
        (c.getNodChop =!= d.getNodChop)
          .option(setNodChopProbeTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideCmd)(d.getNodChop)),
        (c.follow =!= d.follow).option(TcsEpics.instance.aoProbeFollowCmd.setFollowState(encode(d.follow)))
      ).collect{ case Some(x) => x }

      actions.nonEmpty.option{ x => actions.sequence *>
        SeqAction(EpicsTcsConfig.pwfs2OrAowfs.set(Right(d))(x))
      }
    }
    else none

  // Left side is PWFS2, right side is AOWFS
  private def setPwfs2OrAltair(subsystems: NonEmptySet[Subsystem], c: Either[GuiderConfig, ProbeTrackingConfig],
                               d: Either[GuiderConfig@@P2Config, ProbeTrackingConfig@@AoGuide])
  : Option[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] = (c, d) match {
    case (Left(x), Left(y))   => setPwfs2Probe(subsystems, x.tracking, y.tracking)
    case (Right(x), Right(y)) => setAltairProbe(subsystems, x, y)
    case (Right(_), Left(_))  => { _:EpicsTcsConfig => SeqAction.fail[EpicsTcsConfig](SeqexecFailure.Execution(
      "Incompatible configuration: useAO is true but sequence uses PWFS2")) }.some
    case (Left(_), Right(_))  => { _:EpicsTcsConfig => SeqAction.fail[EpicsTcsConfig](SeqexecFailure.Execution(
      "Incompatible configuration: useAO is false but sequence uses Altair")) }.some
  }

  private def setGuiderWfs(on: TcsEpics.WfsObserveCmd, off: EpicsCommand)(c: GuiderSensorOption)
  : SeqAction[Unit] = {
    val NonStopExposures = -1
    c match {
      case GuiderSensorOff => off.mark
      case GuiderSensorOn => on.setNoexp(NonStopExposures) // Set number of exposures to non-stop (-1)
    }
  }

  private val setPwfs1 = setGuiderWfs(TcsEpics.instance.pwfs1ObserveCmd, TcsEpics.instance.pwfs1StopObserveCmd)(_)

  private val setPwfs2 = setGuiderWfs(TcsEpics.instance.pwfs2ObserveCmd, TcsEpics.instance.pwfs2StopObserveCmd)(_)

  private val setOiwfs = setGuiderWfs(TcsEpics.instance.oiwfsObserveCmd, TcsEpics.instance.oiwfsStopObserveCmd)(_)

  val BottomPort: Int = 1
  val InvalidPort: Int = 0

  def portFromSinkName(ports: InstrumentPorts)(n: LightSinkName): Option[Int] = {
    import LightSinkName._
    val port = n match {
      case Gmos |
           Gmos_Ifu => ports.gmosPort
      case Niri_f6 |
           Niri_f14 |
           Niri_f32 => ports.niriPort
      case Nifs     => ports.nifsPort
      case Gnirs    => ports.gnirsPort
      case F2       => ports.flamingos2Port
      case Gpi      => ports.gpiPort
      case Ghost    => ports.ghostPort
      case Gsaoi    => ports.gsaoiPort
      case Ac |
           Hr       => BottomPort
      case Phoenix |
           Visitor  => InvalidPort
    }
    (port =!= InvalidPort).option(port)
  }

  def scienceFoldFromRequested(ports: InstrumentPorts)(r: LightPath): Option[ScienceFold] =
    portFromSinkName(ports)(r.sink).map{ p =>
      if(p === BottomPort && r.source === LightSource.Sky) ScienceFold.Parked
      else ScienceFold.Position(r.source, r.sink, p)
    }

  def setScienceFoldConfig(sfPos: ScienceFold): SeqAction[Unit] = sfPos match {
    case ScienceFold.Parked      => TcsEpics.instance.scienceFoldParkCmd.mark
    case p: ScienceFold.Position => TcsEpics.instance.scienceFoldPosCmd.setScfold(encode(p))
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

  private def calcHrPickupPosition(c: AGConfig, ports: InstrumentPorts): Option[HrwfsPickupPosition] = c.hrwfs.flatMap {
    case HrwfsConfig.Manual(h) => h.some
    case HrwfsConfig.Auto      => scienceFoldFromRequested(ports)(c.sfPos).flatMap {
      case ScienceFold.Parked |
           ScienceFold.Position(_, _, BottomPort) => HrwfsPickupPosition.Parked.some
      case ScienceFold.Position(_, _, _)          => none
    }
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

  private val encodeM2Guide: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue{
      case M2GuideOn(_, _) => "on"
      case M2GuideOff      => "off"
    }

  private val encodeM2Coma: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue{
      case M2GuideOn(ComaOption.ComaOn, _) => "on"
      case _                               => "off"
    }

  private val encodeM2GuideReset: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue{
      case M2GuideOn(_, _) => "off"
      case M2GuideOff      => "on"
    }

  private def setM2Guide(c: M2GuideConfig, d: M2GuideConfig)(tcsCfg: EpicsTcsConfig): SeqAction[EpicsTcsConfig] = {
    val actions = List(
      TcsEpics.instance.m2GuideModeCmd.setComa(encodeM2Coma.encode(d))
        .whenA(encodeM2Coma.encode(d) =!= encodeM2Coma.encode(c)),
      TcsEpics.instance.m2GuideCmd.setState(encodeM2Guide.encode(d))
        .whenA(encodeM2Guide.encode(d) =!= encodeM2Guide.encode(c)),
      TcsEpics.instance.m2GuideConfigCmd.setReset(encodeM2GuideReset.encode(d))
        .whenA(encodeM2GuideReset.encode(d) =!= encodeM2GuideReset.encode(c))
    )

    actions.sequence.unlessA(actions.isEmpty) *> SeqAction(
      (EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m2Guide).set(d)(tcsCfg)
    )
  }

  private def setScienceFold(subsystems: NonEmptySet[Subsystem], c: EpicsTcsConfig, d: LightPath)
  : Option[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] = scienceFoldFromRequested(c.instPorts)(d).flatMap{ sf =>
    (subsystems.contains(Subsystem.AGUnit) && c.scienceFoldPosition.forall(_ =!= sf)).option{ x =>
      setScienceFoldConfig(sf) *> SeqAction(EpicsTcsConfig.scienceFoldPosition.set(sf.some)(x))
    }
  }

  private val tcsTimeout = Seconds(60)
  private val agTimeout = Seconds(60)

  private def willMove(current: EpicsTcsConfig, demand: TcsConfig): Boolean =
    demand.tc.offsetA.exists(_ =!= current.instrumentOffset)

  private def applyParam[T: Eq](used: Boolean,
                                current: T,
                                demand: T,
                                act: T => SeqAction[Unit],
                                lens: Lens[EpicsTcsConfig, T])
  : Option[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] =
    (used && current =!= demand).option(c => act(demand) *> SeqAction(lens.set(demand)(c)))

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Either[Altair[IO], Gems[IO]]],
                           tcs: TcsConfig): SeqAction[Unit] = {
    def configParams(current: EpicsTcsConfig): List[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] = List(
      setPwfs1Probe(subsystems, current.pwfs1.tracking, tcs.gds.pwfs1.tracking),
      setPwfs2OrAltair(subsystems, current.pwfs2OrAowfs, tcs.gds.pwfs2OrAowfs),
      setOiwfsProbe(subsystems, current.oiwfs.tracking, tcs.gds.oiwfs.tracking),
      tcs.tc.offsetA.flatMap(o => applyParam(subsystems.contains(Subsystem.Mount), current.offset,
        o.toFocalPlaneOffset(current.iaa), setTelescopeOffset, EpicsTcsConfig.offset
      )),
      tcs.tc.wavelA.flatMap(applyParam(subsystems.contains(Subsystem.Mount), current.wavelA, _, setWavelength,
        EpicsTcsConfig.wavelA
      )),
      setScienceFold(subsystems, current, tcs.agc.sfPos),
      calcHrPickupPosition(tcs.agc, current.instPorts).flatMap(applyParam(subsystems.contains(Subsystem.AGUnit),
        current.hrwfsPickupPosition, _, setHRPickupConfig, EpicsTcsConfig.hrwfsPickupPosition)
      )
    ).collect{ case Some(x) => x }

    def sysConfig(current: EpicsTcsConfig): SeqAction[EpicsTcsConfig] = {
      val params = configParams(current)

      if(params.nonEmpty)
        for {
          s <- params.foldLeft(SeqAction(current)){ case (c, p) => c.flatMap(p) }
          _ <- TcsEpics.instance.post
          _ <- SeqAction(Log.debug("TCS configuration command post"))
          _ <- if(subsystems.contains(Subsystem.Mount))
                 TcsEpics.instance.waitInPosition(tcsTimeout) *> EitherT.right(IO.apply(Log.info("TCS inposition")))
               else if(Set(Subsystem.PWFS1, Subsystem.PWFS2, Subsystem.AGUnit).exists(subsystems.contains))
                 TcsEpics.instance.waitAGInPosition(agTimeout) *> EitherT.right(IO.apply(Log.debug("AG inposition")))
               else SeqAction.void
        } yield s
      else
        SeqAction(Log.debug("Skipping TCS configuration")) *> SeqAction(current)
    }

    for {
      s0 <- TcsConfigRetriever.retrieveConfiguration(gaos.flatMap(_.swap.toOption.map(_.isFollowing))
              .getOrElse(IO(false.some)))
      pr <- pauseResumeGaos(gaos, s0, tcs)
      _  <- SeqAction.lift(pr.pause.getOrElse(IO.unit))
      s1 <- guideOff(subsystems, s0, tcs, pr.pause.isEmpty)
      s2 <- sysConfig(s1)
      _  <- guideOn(subsystems, s2, tcs, pr.resume.isDefined)
      _  <- SeqAction.lift(pr.resume.getOrElse(IO.unit)) //resume Gaos
    } yield ()
  }

  def configurePwfs2Detector(subsystems: NonEmptySet[Subsystem], c: Either[GuiderConfig, ProbeTrackingConfig],
                             d: Either[GuiderConfig@@P2Config, ProbeTrackingConfig@@AoGuide])
  : Option[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] = for {
    cur <- c.swap.toOption
    dem <- d.swap.toOption
    if subsystems.contains(Subsystem.PWFS2) && (cur.detector =!= dem.detector)
  } yield { cfg: EpicsTcsConfig => setPwfs2(dem.detector)
    .map(_ => EpicsTcsConfig.pwfs2OrAowfs.modify(_.leftMap(GuiderConfig.detector.set(dem.detector)))(cfg))
  }

  def guideParams(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsConfig)
  : List[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] = List(
      applyParam(subsystems.contains(Subsystem.Mount), current.telescopeGuideConfig.mountGuide, demand.gc.mountGuide,
        setMountGuide, EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.mountGuide),
      applyParam(subsystems.contains(Subsystem.M1), current.telescopeGuideConfig.m1Guide, demand.gc.m1Guide,
        setM1Guide, EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m1Guide),
      subsystems.contains(Subsystem.M2).option(setM2Guide(current.telescopeGuideConfig.m2Guide, demand.gc.m2Guide)(_)),
      applyParam(subsystems.contains(Subsystem.PWFS1), current.pwfs1.detector, demand.gds.pwfs1.detector,
        setPwfs1, EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector),
      configurePwfs2Detector(subsystems, current.pwfs2OrAowfs, demand.gds.pwfs2OrAowfs),
      applyParam(subsystems.contains(Subsystem.OIWFS), current.oiwfs.detector, demand.gds.oiwfs.detector,
        setOiwfs, EpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector)
    ).collect{ case Some(x) => x }


  def tagIso[B, T]: Iso[B@@T, B] = Iso.apply[B@@T, B](x => x)(tag[T](_))

  def calcGuideOff(current: EpicsTcsConfig, demand: TcsConfig, gaosEnabled: Boolean): TcsConfig = {
    val mustOff = willMove(current, demand)
    // Only turn things off here. Things that must be turned on will be turned on in GuideOn.
    def calc(c: GuiderSensorOption, d: GuiderSensorOption) = (mustOff || d === GuiderSensorOff).fold(GuiderSensorOff, c)

    def calcPwfs2(c: Either[GuiderConfig, ProbeTrackingConfig])(
      d: Either[GuiderConfig@@P2Config, ProbeTrackingConfig@@AoGuide])
    : Either[GuiderConfig@@P2Config, ProbeTrackingConfig@@AoGuide] = (c, d) match {
      case (Left(x), Left(y)) => Left((tagIso ^|-> GuiderConfig.detector).set(calc(x.detector, y.detector))(y))
      case _                  => d
    }

    (TcsConfig.gds.modify(
      (GuidersConfig.pwfs1 ^<-> tagIso ^|-> GuiderConfig.detector)
        .set(calc(current.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
        GuidersConfig.pwfs2OrAowfs.modify(calcPwfs2(current.pwfs2OrAowfs)) >>>
        (GuidersConfig.oiwfs ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.oiwfs.detector, demand.gds.oiwfs.detector))
    ) >>> TcsConfig.gc.modify(
      TelescopeGuideConfig.mountGuide.set(
        (mustOff || demand.gc.mountGuide === MountGuideOff).fold(MountGuideOff, current.telescopeGuideConfig.mountGuide)
      ) >>>
        TelescopeGuideConfig.m1Guide.set(
          (mustOff || demand.gc.m1Guide === M1GuideOff).fold(M1GuideOff, current.telescopeGuideConfig.m1Guide)
        ) >>>
        TelescopeGuideConfig.m2Guide.set(
          (mustOff || demand.gc.m2Guide === M2GuideOff).fold(M2GuideOff, current.telescopeGuideConfig.m2Guide)
        )
    ) >>> normalizeM1Guiding(gaosEnabled) >>> normalizeM2Guiding(gaosEnabled) >>> normalizeMountGuiding)(demand)
  }

  def calcAoPauseConditions(current: EpicsTcsConfig, demand: TcsConfig): Set[PauseCondition] = Set(
    demand.tc.offsetA.flatMap(v => (v =!= current.instrumentOffset)
      .option(OffsetMove(current.offset, v.toFocalPlaneOffset(current.iaa)))),
    (current.oiwfs.detector === GuiderSensorOn && demand.gds.oiwfs.detector === GuiderSensorOff).option(OiOff),
    (current.pwfs1.detector === GuiderSensorOn && demand.gds.pwfs1.detector === GuiderSensorOff).option(P1Off),
    demand.gds.pwfs2OrAowfs.toOption.filter(_.follow === FollowOption.FollowOff).as(GaosStarOff)
  ).collect{ case Some(x) => x }

  def calcAoResumeConditions(current: EpicsTcsConfig, demand: TcsConfig): Set[ResumeCondition] = Set(
    demand.tc.offsetA.map(v => OffsetReached(v.toFocalPlaneOffset(current.iaa))),
    (demand.gds.oiwfs.detector === GuiderSensorOn).option(OiOn),
    (demand.gds.pwfs1.detector === GuiderSensorOn).option(P1On),
    demand.gds.pwfs2OrAowfs.toOption.filter(_.follow === FollowOption.FollowOn).as(GaosStarOn)
  ).collect{ case Some(x) => x }

  def pauseResumeGaos(gaos: Option[Either[Altair[IO], Gems[IO]]], current: EpicsTcsConfig, demand: TcsConfig)
  : SeqAction[PauseResume[IO]] = (gaos, demand.gaos).mapN {
    case (Left(g), c)  => SeqActionF.embed[IO, Gaos.PauseResume[IO]](
      g.pauseResume(c, calcAoPauseConditions(current, demand), calcAoResumeConditions(current, demand)))
    case (Right(_), _) => SeqAction.fail[PauseResume[IO]](
      SeqexecFailure.Unexpected("GeMS not supported"))
    case _             => SeqAction(PauseResume[IO](None, None))
  }.getOrElse(SeqAction(PauseResume(None, None)))

  def updateEpicsGuideConfig(epicsCfg: EpicsTcsConfig, demand: TcsConfig): EpicsTcsConfig = (
    EpicsTcsConfig.telescopeGuideConfig.set(demand.gc) >>>
      (EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector).set(demand.gds.pwfs1.detector) >>>
      EpicsTcsConfig.pwfs2OrAowfs.modify{ c => (c, demand.gds.pwfs2OrAowfs) match {
        case (Left(x), Left(y)) => Left(GuiderConfig.detector.set(y.detector)(x))
        case _                  => c
      }} >>>
      (EpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector).set(demand.gds.oiwfs.detector)
  )(epicsCfg)

  def guideOff(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsConfig, pauseGaos: Boolean)
  : SeqAction[EpicsTcsConfig] = {
    val params = guideParams(subsystems, current, calcGuideOff(current, demand, pauseGaos) )

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(SeqAction(current)){ case (c, p) => c.flatMap(p)}
        _ <- TcsEpics.instance.post
        _ <- SeqAction(Log.info("Turning guide off"))
      } yield s
    else
      SeqAction(Log.info("Skipping guide off")) *> SeqAction(current)
  }

  def guideOn(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsConfig, gaosEnabled: Boolean)
  : SeqAction[EpicsTcsConfig] = {

    // If the demand turned off any WFS, normalize will turn off the corresponding processing
    val normalizedGuiding = (normalizeM1Guiding(gaosEnabled) >>> normalizeM2Guiding(gaosEnabled) >>>
      normalizeMountGuiding)(demand)

    val params = guideParams(subsystems, current, normalizedGuiding)

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(SeqAction(current)){ case (c, p) => c.flatMap(p)}
        _ <- TcsEpics.instance.post
        _ <- SeqAction(Log.info("Turning guide on"))
      } yield s
    else
      SeqAction(Log.info("Skipping guide on")) *> SeqAction(current)
  }

  override def notifyObserveStart: SeqAction[Unit] =
    TcsEpics.instance.observe.mark *> TcsEpics.instance.post.void

  override def notifyObserveEnd: SeqAction[Unit] = TcsEpics.instance.endObserve.mark *> TcsEpics.instance.post.void

  /* AO fold position */
  sealed trait AoFold {
    val active: Boolean
  }
  object AoFold {
    object In extends AoFold {
      override val active: Boolean = true
    }
    object Out extends AoFold {
      override val active: Boolean = false
    }
  }

  final case class InstrumentPorts(
    flamingos2Port: Int,
    ghostPort: Int,
    gmosPort: Int,
    gnirsPort: Int,
    gpiPort: Int,
    gsaoiPort: Int,
    nifsPort: Int,
    niriPort: Int
  )

  sealed trait ScienceFold

  object ScienceFold {
    case object Parked extends ScienceFold
    final case class Position(source: LightSource, sink: LightSinkName, port: Int) extends ScienceFold

    implicit val positionEq: Eq[Position] = Eq.by(x => (x.source, x.sink, x.port))

    implicit val eq: Eq[ScienceFold] = Eq.instance{
      case (Parked, Parked)           => true
      case (a: Position, b: Position) => a === b
      case _                          => false
    }
  }

  @Lenses
  final case class EpicsTcsConfig(
    iaa: Angle,
    offset: FocalPlaneOffset,
    wavelA: Wavelength,
    pwfs1: GuiderConfig,
    pwfs2OrAowfs: Either[GuiderConfig, ProbeTrackingConfig],
    oiwfs: GuiderConfig,
    telescopeGuideConfig: TelescopeGuideConfig,
    aoFold: AoFold,
    scienceFoldPosition: Option[ScienceFold],
    hrwfsPickupPosition: HrwfsPickupPosition,
    instPorts: InstrumentPorts
  ) {
    val instrumentOffset: InstrumentOffset = offset.toInstrumentOffset(iaa)
  }

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object EpicsTcsConfig

  def guiderActive(c: GuiderConfig): Boolean = c.tracking match {
    case ProbeTrackingConfig.On(_) => c.detector === GuiderSensorOn
    case _                         => false
  }

  // Disable M1 guiding if source is off
  def normalizeM1Guiding(gaosEnabled: Boolean): Endo[TcsConfig] = cfg =>
    (TcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
      case g@M1GuideOn(src) => src match {
        case M1Source.PWFS1 => if(guiderActive(cfg.gds.pwfs1)) g else M1GuideOff
        case M1Source.PWFS2 => if(cfg.gds.pwfs2OrAowfs.swap.exists(guiderActive)) g else M1GuideOff
        case M1Source.OIWFS => if(guiderActive(cfg.gds.oiwfs)) g else M1GuideOff
        case M1Source.GAOS  => if(cfg.gds.pwfs2OrAowfs.exists(_.follow === FollowOn) && gaosEnabled) g else M1GuideOff
        case _              => g
      }
      case x                => x
    }(cfg)

  // Disable M2 sources if they are off, disable M2 guiding if all are off
  def normalizeM2Guiding(gaosEnabled: Boolean): Endo[TcsConfig] = cfg =>
    (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).modify{
      case M2GuideOn(coma, srcs) =>
        val ss = srcs.filter{
          case TipTiltSource.PWFS1 => guiderActive(cfg.gds.pwfs1)
          case TipTiltSource.PWFS2 => cfg.gds.pwfs2OrAowfs.swap.map(guiderActive).getOrElse(false)
          case TipTiltSource.OIWFS => guiderActive(cfg.gds.oiwfs)
          case TipTiltSource.GAOS  => cfg.gds.pwfs2OrAowfs.exists(_.follow === FollowOn) && gaosEnabled
          case _                   => true
        }
        if(ss.isEmpty) M2GuideOff
        else M2GuideOn((cfg.gc.m1Guide =!= M1GuideOff).fold(coma, ComaOption.ComaOff), ss)
      case x                     => x
    }(cfg)

  // Disable Mount guiding if M2 guiding is disabled
  val normalizeMountGuiding: Endo[TcsConfig] = cfg =>
    (TcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
      case (MountGuideOn, M2GuideOn(_, _)) => MountGuideOn
      case _                               => MountGuideOff
    } }(cfg)

  final case class GuideControl(subs: Subsystem,
                          parkCmd: EpicsCommand,
                          nodChopGuideCmd: ProbeGuideCmd,
                          followCmd: ProbeFollowCmd
                         )

}
