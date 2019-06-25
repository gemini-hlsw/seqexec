// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data._
import cats.effect.{IO, Sync}
import cats.implicits._
import edu.gemini.spModel.core.Wavelength
import org.log4s.{Logger, getLogger}
import mouse.boolean._
import cats.Endo
import monocle.macros.Lenses
import squants.{Angle, Length}
import seqexec.model.enum.Instrument
import seqexec.model.enum.ComaOption
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.M1Source
import seqexec.model.enum.TipTiltSource
import seqexec.model.M2GuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.server.altair.Altair
import seqexec.server.tcs.TcsController._
import seqexec.server.{EpicsCodex, SeqexecFailure}
import seqexec.server.tcs.Gaos.{GaosGuideOff, GaosGuideOn, OffsetMove, OffsetReached, OiOff, OiOn, P1Off, P1On, PauseCondition, PauseResume, ResumeCondition}
import seqexec.server.tcs.TcsNorthController.{GuidersConfig, TcsNorthConfig}
import seqexec.server.tcs.TcsControllerEpics._
import shapeless.tag.@@
import squants.space.Arcseconds

class TcsNorthControllerEpics private extends TcsControllerEpics[TcsNorthControllerEpics.EpicsTcsConfig] with TcsNorthController[IO] {

  import TcsNorthControllerEpics._
  import EpicsCodex._

  private def setAltairProbe(subsystems: NonEmptySet[Subsystem], c: ProbeTrackingConfig, d: ProbeTrackingConfig)
  : Option[EpicsTcsConfig => IO[EpicsTcsConfig]] =
    if(subsystems.contains(Subsystem.Gaos)) {
      val actions = List(
        (c.getNodChop =!= d.getNodChop)
          .option(setNodChopProbeTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideCmd)(d.getNodChop)),
        (c.follow =!= d.follow).option(TcsEpics.instance.aoProbeFollowCmd.setFollowState(encode(d.follow)))
      ).collect{ case Some(x) => x }

      actions.nonEmpty.option{ x => actions.sequence *>
        IO(EpicsTcsConfig.pwfs2OrAowfs.set(Right(d))(x))
      }
    }
    else none

  // Left side is PWFS2, right side is AOWFS
  private def setPwfs2OrAltair(subsystems: NonEmptySet[Subsystem], c: Either[GuiderConfig, ProbeTrackingConfig],
                               d: Either[GuiderConfig@@P2Config, GuiderConfig@@AoGuide])
  : Option[EpicsTcsConfig => IO[EpicsTcsConfig]] = (c, d) match {
    case (Left(x), Left(y))   => setPwfs2Probe{v =>
      EpicsTcsConfig.pwfs2OrAowfs.modify(_.leftMap(GuiderConfig.tracking.set(v)))
    }(subsystems, x.tracking, y.tracking)
    case (Right(x), Right(y)) => setAltairProbe(subsystems, x, y.tracking)
    case (Right(_), Left(_))  => { _:EpicsTcsConfig => IO.raiseError(SeqexecFailure.Execution(
      "Incompatible configuration: useAO is true but sequence uses PWFS2")) }.some
    case (Left(_), Right(_))  => { _:EpicsTcsConfig => IO.raiseError(SeqexecFailure.Execution(
      "Incompatible configuration: useAO is false but sequence uses Altair")) }.some
  }

  private def aoOffsetThreshold(instrument: Instrument): Option[Length] = instrument match {
    case Instrument.Nifs  => (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
    case Instrument.Niri  => (Arcseconds(3.0)/FOCAL_PLANE_SCALE).some
    case Instrument.Gnirs => (Arcseconds(3.0)/FOCAL_PLANE_SCALE).some
    case _                => none
  }

  private def mustPauseWhileOffsetting(current: EpicsTcsConfig, demand: TcsNorthConfig): Boolean = {
    val distanceSquared = demand.tc.offsetA.map(_.toFocalPlaneOffset(current.iaa))
      .map { o => (o.x - current.offset.x, o.y - current.offset.y) }
      .map(d => d._1 * d._1 + d._2 * d._2)

    val thresholds = List(
      (Tcs.calcGuiderInUse(demand.gc, TipTiltSource.PWFS1, M1Source.PWFS1) && demand.gds.pwfs1.isActive)
        .option(pwfs1OffsetThreshold),
      (Tcs.calcGuiderInUse(demand.gc, TipTiltSource.PWFS2, M1Source.PWFS2) &&
        demand.gds.pwfs2OrAowfs.swap.exists(_.isActive))
        .option(pwfs2OffsetThreshold),
      demand.inst.oiOffsetGuideThreshold
        .filter(_ => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.OIWFS, M1Source.OIWFS) && demand.gds.oiwfs.isActive),
      aoOffsetThreshold(demand.inst.instrument)
        .filter(_ => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.GAOS, M1Source.GAOS) &&
          demand.gds.pwfs2OrAowfs.exists(_.isActive)
        )
    )
    // Does the offset movement surpass any of the existing thresholds ?
    distanceSquared.exists(dd => thresholds.exists(_.exists(t => t*t < dd)))
  }

  override def applyConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Option[Altair[IO]],
                           tcs: TcsNorthConfig): IO[Unit] = {
    def configParams(current: EpicsTcsConfig): List[EpicsTcsConfig => IO[EpicsTcsConfig]] = List(
      setPwfs1Probe((EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.tracking).set(_))(subsystems, current.pwfs1.tracking, tcs.gds.pwfs1.tracking),
      setPwfs2OrAltair(subsystems, current.pwfs2OrAowfs, tcs.gds.pwfs2OrAowfs),
      setOiwfsProbe((EpicsTcsConfig.oiwfs ^|-> GuiderConfig.tracking).set(_))(subsystems, current.oiwfs.tracking, tcs.gds.oiwfs.tracking),
      tcs.tc.offsetA.flatMap(o => applyParam(subsystems.contains(Subsystem.Mount), current.offset,
        o.toFocalPlaneOffset(current.iaa), setTelescopeOffset, EpicsTcsConfig.offset
      )),
      tcs.tc.wavelA.flatMap(applyParam(subsystems.contains(Subsystem.Mount), current.wavelA, _, setWavelength,
        EpicsTcsConfig.wavelA
      )),
      setScienceFold(subsystems, current, tcs.agc.sfPos, EpicsTcsConfig.scienceFoldPosition, current.instPorts),
      setHrPickup(subsystems, tcs.agc, current, EpicsTcsConfig.hrwfsPickupPosition, current.instPorts)
    ).collect{ case Some(x) => x }

    def sysConfig(current: EpicsTcsConfig): IO[EpicsTcsConfig] = {
      val params = configParams(current)

      if(params.nonEmpty)
        for {
          s <- params.foldLeft(IO(current)){ case (c, p) => c.flatMap(p) }
          _ <- TcsEpics.instance.post
          _ <- IO(Log.debug("TCS configuration command post"))
          _ <- if(subsystems.contains(Subsystem.Mount))
            TcsEpics.instance.waitInPosition(tcsTimeout) *> IO.apply(Log.info("TCS inposition"))
          else if(Set(Subsystem.PWFS1, Subsystem.PWFS2, Subsystem.AGUnit).exists(subsystems.contains))
            TcsEpics.instance.waitAGInPosition(agTimeout) *> IO.apply(Log.debug("AG inposition"))
          else IO.unit
        } yield s
      else
        IO(Log.debug("Skipping TCS configuration")) *> IO(current)
    }

    for {
      s0 <- TcsConfigRetriever.retrieveConfigurationNorth(gaos.map(_.isFollowing)
        .getOrElse(IO(false.some)))
      pr <- pauseResumeGaos[IO](gaos, s0, tcs)
      _  <- pr.pause.getOrElse(IO.unit)
      s1 <- guideOff(subsystems, s0, tcs, pr.pause.isEmpty)
      s2 <- sysConfig(s1)
      _  <- guideOn(subsystems, s2, tcs, pr.resume.isDefined)
      _  <- pr.resume.getOrElse(IO.unit) //resume Gaos
    } yield ()
  }

  def configurePwfs2Detector(subsystems: NonEmptySet[Subsystem], c: Either[GuiderConfig, ProbeTrackingConfig],
                             d: Either[GuiderConfig@@P2Config, GuiderConfig@@AoGuide])
  : Option[EpicsTcsConfig => IO[EpicsTcsConfig]] = for {
    cur <- c.swap.toOption
    dem <- d.swap.toOption
    if subsystems.contains(Subsystem.PWFS2) && (cur.detector =!= dem.detector)
  } yield { cfg: EpicsTcsConfig => setPwfs2(dem.detector)
    .map(_ => EpicsTcsConfig.pwfs2OrAowfs.modify(_.leftMap(GuiderConfig.detector.set(dem.detector)))(cfg))
  }

  def guideParams(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsNorthConfig)
  : List[EpicsTcsConfig => IO[EpicsTcsConfig]] = List(
    applyParam(subsystems.contains(Subsystem.Mount), current.telescopeGuideConfig.mountGuide, demand.gc.mountGuide,
      setMountGuide, EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.mountGuide),
    applyParam(subsystems.contains(Subsystem.M1), current.telescopeGuideConfig.m1Guide, demand.gc.m1Guide,
      setM1Guide, EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m1Guide),
    subsystems.contains(Subsystem.M2).option(setM2Guide(current.telescopeGuideConfig.m2Guide, demand.gc.m2Guide,
      EpicsTcsConfig.telescopeGuideConfig)(_)),
    applyParam(subsystems.contains(Subsystem.PWFS1), current.pwfs1.detector, demand.gds.pwfs1.detector,
      setPwfs1, EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector),
    configurePwfs2Detector(subsystems, current.pwfs2OrAowfs, demand.gds.pwfs2OrAowfs),
    applyParam(subsystems.contains(Subsystem.OIWFS), current.oiwfs.detector, demand.gds.oiwfs.detector,
      setOiwfs, EpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector)
  ).collect{ case Some(x) => x }

  def calcGuideOff(current: EpicsTcsConfig, demand: TcsNorthConfig, gaosEnabled: Boolean): TcsNorthConfig = {
    val mustOff = mustPauseWhileOffsetting(current, demand)
    // Only turn things off here. Things that must be turned on will be turned on in GuideOn.
    def calc(c: GuiderSensorOption, d: GuiderSensorOption) = (mustOff || d === GuiderSensorOff).fold(GuiderSensorOff, c)

    def calcPwfs2(c: Either[GuiderConfig, ProbeTrackingConfig])(
      d: Either[GuiderConfig@@P2Config, GuiderConfig@@AoGuide])
    : Either[GuiderConfig@@P2Config, GuiderConfig@@AoGuide] = (c, d) match {
      case (Left(x), Left(y)) => Left((tagIso ^|-> GuiderConfig.detector).set(calc(x.detector, y.detector))(y))
      case _                  => d
    }

    (TcsNorthConfig.gds.modify(
      (GuidersConfig.pwfs1 ^<-> tagIso ^|-> GuiderConfig.detector)
        .set(calc(current.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
        GuidersConfig.pwfs2OrAowfs.modify(calcPwfs2(current.pwfs2OrAowfs)) >>>
        (GuidersConfig.oiwfs ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.oiwfs.detector, demand.gds.oiwfs.detector))
    ) >>> TcsNorthConfig.gc.modify(
      TelescopeGuideConfig.mountGuide.set(
        (mustOff || demand.gc.mountGuide === MountGuideOption.MountGuideOff).fold(MountGuideOption.MountGuideOff, current.telescopeGuideConfig.mountGuide)
      ) >>>
        TelescopeGuideConfig.m1Guide.set(
          (mustOff || demand.gc.m1Guide === M1GuideConfig.M1GuideOff).fold(M1GuideConfig.M1GuideOff, current.telescopeGuideConfig.m1Guide)
        ) >>>
        TelescopeGuideConfig.m2Guide.set(
          (mustOff || demand.gc.m2Guide === M2GuideConfig.M2GuideOff).fold(M2GuideConfig.M2GuideOff, current.telescopeGuideConfig.m2Guide)
        )
    ) >>> normalizeM1Guiding(gaosEnabled) >>> normalizeM2Guiding(gaosEnabled) >>> normalizeMountGuiding)(demand)
  }

  def calcAoPauseConditions(current: EpicsTcsConfig, demand: TcsNorthConfig): Set[PauseCondition] = Set(
    demand.tc.offsetA.flatMap(v => (v =!= current.instrumentOffset)
      .option(OffsetMove(current.offset, v.toFocalPlaneOffset(current.iaa)))),
    (current.oiwfs.detector === GuiderSensorOn && demand.gds.oiwfs.detector === GuiderSensorOff).option(OiOff),
    (current.pwfs1.detector === GuiderSensorOn && demand.gds.pwfs1.detector === GuiderSensorOff).option(P1Off),
    demand.gds.pwfs2OrAowfs.toOption.filter(_.detector === GuiderSensorOff).as(GaosGuideOff)
  ).collect{ case Some(x) => x }

  def calcAoResumeConditions(current: EpicsTcsConfig, demand: TcsNorthConfig): Set[ResumeCondition] = Set(
    demand.tc.offsetA.map(v => OffsetReached(v.toFocalPlaneOffset(current.iaa))),
    (demand.gds.oiwfs.detector === GuiderSensorOn).option(OiOn),
    (demand.gds.pwfs1.detector === GuiderSensorOn).option(P1On),
    demand.gds.pwfs2OrAowfs.toOption.filter(_.detector === GuiderSensorOn).as(GaosGuideOn)
  ).collect{ case Some(x) => x }

  def pauseResumeGaos[F[_]: Sync ](gaos: Option[Altair[F]], current: EpicsTcsConfig, demand: TcsNorthConfig)
  : F[PauseResume[F]] = (gaos, demand.gaos).mapN {
    case (g, c)  => g.pauseResume(c, calcAoPauseConditions(current, demand), calcAoResumeConditions(current, demand))
  }.getOrElse(PauseResume[F](None, None).pure[F])

  def updateEpicsGuideConfig(epicsCfg: EpicsTcsConfig, demand: TcsNorthConfig): EpicsTcsConfig = (
    EpicsTcsConfig.telescopeGuideConfig.set(demand.gc) >>>
      (EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector).set(demand.gds.pwfs1.detector) >>>
      EpicsTcsConfig.pwfs2OrAowfs.modify{ c => (c, demand.gds.pwfs2OrAowfs) match {
        case (Left(x), Left(y)) => Left(GuiderConfig.detector.set(y.detector)(x))
        case _                  => c
      }} >>>
      (EpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector).set(demand.gds.oiwfs.detector)
    )(epicsCfg)

  def guideOff(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsNorthConfig, pauseGaos: Boolean)
  : IO[EpicsTcsConfig] = {
    val params = guideParams(subsystems, current, calcGuideOff(current, demand, pauseGaos) )

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(IO(current)){ case (c, p) => c.flatMap(p)}
        _ <- TcsEpics.instance.post
        _ <- IO(Log.info("Turning guide off"))
      } yield s
    else
      IO(Log.info("Skipping guide off")) *> IO(current)
  }

  def guideOn(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsNorthConfig, gaosEnabled: Boolean)
  : IO[EpicsTcsConfig] = {

    // If the demand turned off any WFS, normalize will turn off the corresponding processing
    val normalizedGuiding = (normalizeM1Guiding(gaosEnabled) >>> normalizeM2Guiding(gaosEnabled) >>>
      normalizeMountGuiding)(demand)

    val params = guideParams(subsystems, current, normalizedGuiding)

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(IO(current)){ case (c, p) => c.flatMap(p)}
        _ <- TcsEpics.instance.post
        _ <- IO(Log.info("Turning guide on"))
      } yield s
    else
      IO(Log.info("Skipping guide on")) *> IO(current)
  }

  override def notifyObserveStart: IO[Unit] =
    TcsEpics.instance.observe.mark[IO] *> TcsEpics.instance.post.void

  override def notifyObserveEnd: IO[Unit] = TcsEpics.instance.endObserve.mark[IO] *> TcsEpics.instance.post.void

  def guiderActive(c: GuiderConfig): Boolean = c.tracking match {
    case ProbeTrackingConfig.On(_) => c.detector === GuiderSensorOn
    case _                         => false
  }

  // Disable M1 guiding if source is off
  def normalizeM1Guiding(gaosEnabled: Boolean): Endo[TcsNorthConfig] = cfg =>
    (TcsNorthConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
      case g @ M1GuideConfig.M1GuideOn(src) => src match {
        case M1Source.PWFS1 => if(guiderActive(cfg.gds.pwfs1)) g else M1GuideConfig.M1GuideOff
        case M1Source.PWFS2 => if(cfg.gds.pwfs2OrAowfs.swap.exists(guiderActive)) g else M1GuideConfig.M1GuideOff
        case M1Source.OIWFS => if(guiderActive(cfg.gds.oiwfs)) g else M1GuideConfig.M1GuideOff
        case M1Source.GAOS  => if(cfg.gds.pwfs2OrAowfs.exists(_.detector === GuiderSensorOn) && gaosEnabled) g
        else M1GuideConfig.M1GuideOff
        case _              => g
      }
      case x                => x
    }(cfg)

  // Disable M2 sources if they are off, disable M2 guiding if all are off
  def normalizeM2Guiding(gaosEnabled: Boolean): Endo[TcsNorthConfig] = cfg =>
    (TcsNorthConfig.gc ^|-> TelescopeGuideConfig.m2Guide).modify{
      case M2GuideConfig.M2GuideOn(coma, srcs) =>
        val ss = srcs.filter{
          case TipTiltSource.PWFS1 => guiderActive(cfg.gds.pwfs1)
          case TipTiltSource.PWFS2 => cfg.gds.pwfs2OrAowfs.swap.map(guiderActive).getOrElse(false)
          case TipTiltSource.OIWFS => guiderActive(cfg.gds.oiwfs)
          case TipTiltSource.GAOS  => cfg.gds.pwfs2OrAowfs.exists(_.detector === GuiderSensorOn) && gaosEnabled
          case _                   => true
        }
        if(ss.isEmpty) M2GuideConfig.M2GuideOff
        else M2GuideConfig.M2GuideOn((cfg.gc.m1Guide =!= M1GuideConfig.M1GuideOff).fold(coma, ComaOption.ComaOff), ss)
      case x                     => x
    }(cfg)

  // Disable Mount guiding if M2 guiding is disabled
  val normalizeMountGuiding: Endo[TcsNorthConfig] = cfg =>
    (TcsNorthConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
      case (MountGuideOption.MountGuideOn, M2GuideConfig.M2GuideOn(_, _)) => MountGuideOption.MountGuideOn
      case _                                                              => MountGuideOption.MountGuideOff
    } }(cfg)

}

object TcsNorthControllerEpics {
  val Log: Logger = getLogger

  def apply(): TcsNorthController[IO] = new TcsNorthControllerEpics

//  /* AO fold position */
//  sealed trait AoFold {
//    val active: Boolean
//  }
//  object AoFold {
//    object In extends AoFold {
//      override val active: Boolean = true
//    }
//    object Out extends AoFold {
//      override val active: Boolean = false
//    }
//  }
//
//  final case class InstrumentPorts(
//                                    flamingos2Port: Int,
//                                    ghostPort: Int,
//                                    gmosPort: Int,
//                                    gnirsPort: Int,
//                                    gpiPort: Int,
//                                    gsaoiPort: Int,
//                                    nifsPort: Int,
//                                    niriPort: Int
//                                  )
//
//  val BottomPort: Int = 1
//  val InvalidPort: Int = 0

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

  object EpicsTcsConfig

//  final case class GuideControl[F[_]: Async](subs: Subsystem,
//                                             parkCmd: EpicsCommandF,
//                                             nodChopGuideCmd: ProbeGuideCmd[F],
//                                             followCmd: ProbeFollowCmd[F]
//                                            )
//
//  val pwfs1OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE
//  val pwfs2OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE

}
