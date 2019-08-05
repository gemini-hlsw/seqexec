// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Endo
import cats.data._
import cats.effect.{IO, Sync}
import cats.implicits._
import org.log4s.{Logger, getLogger}
import mouse.boolean._
import monocle.Iso
import monocle.macros.Lenses
import squants.Length
import seqexec.model.enum.Instrument
import seqexec.model.enum.ComaOption
import seqexec.model.enum.MountGuideOption
import seqexec.model.enum.M1Source
import seqexec.model.enum.TipTiltSource
import seqexec.model.M2GuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.tcs.TcsController._
import seqexec.server.EpicsCodex.encode
import seqexec.server.SeqexecFailure
import seqexec.server.tcs.Gaos._
import seqexec.server.tcs.TcsControllerEpicsCommon._
import seqexec.server.tcs.TcsNorthController.TcsNorthAoConfig
import shapeless.tag
import shapeless.tag.@@
import squants.space.Arcseconds

object TcsNorthControllerEpicsAo {

  val Log: Logger = getLogger

  private def setAltairProbe(subsystems: NonEmptySet[Subsystem], c: ProbeTrackingConfig, d: ProbeTrackingConfig)
  : Option[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] =
    if(subsystems.contains(Subsystem.Gaos)) {
      val actions = List(
        (c.getNodChop =!= d.getNodChop)
          .option(setNodChopProbeTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideCmd)(d.getNodChop)),
        (c.follow =!= d.follow).option(TcsEpics.instance.aoProbeFollowCmd.setFollowState(encode(d.follow)))
      ).collect{ case Some(x) => x }

      actions.nonEmpty.option{ x => actions.sequence *>
        IO(EpicsTcsAoConfig.aowfs.set(d)(x))
      }
    }
    else none

  private def aoOffsetThreshold(instrument: Instrument): Option[Length] = instrument match {
    case Instrument.Nifs  => (Arcseconds(0.01)/FOCAL_PLANE_SCALE).some
    case Instrument.Niri  => (Arcseconds(3.0)/FOCAL_PLANE_SCALE).some
    case Instrument.Gnirs => (Arcseconds(3.0)/FOCAL_PLANE_SCALE).some
    case _                => none
  }

  private def mustPauseWhileOffsetting(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig): Boolean = {
    val distanceSquared = demand.tc.offsetA.map(_.toFocalPlaneOffset(current.base.iaa))
      .map { o => (o.x - current.base.offset.x, o.y - current.base.offset.y) }
      .map(d => d._1 * d._1 + d._2 * d._2)

    val aoThreshold = aoOffsetThreshold(demand.inst.instrument)
        .filter(_ => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.GAOS, M1Source.GAOS) && demand.gds.aoguide.isActive)

    val thresholds = List(
      (Tcs.calcGuiderInUse(demand.gc, TipTiltSource.PWFS1, M1Source.PWFS1) && demand.gds.pwfs1.isActive)
        .option(pwfs1OffsetThreshold),
      aoThreshold,
      demand.inst.oiOffsetGuideThreshold
        .filter(_ => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.OIWFS, M1Source.OIWFS) && demand.gds.oiwfs.isActive)
    )
    // Does the offset movement surpass any of the existing thresholds ?
    distanceSquared.exists(dd => thresholds.exists(_.exists(t => t*t < dd)))
  }

  def applyAoConfig(subsystems: NonEmptySet[Subsystem],
                           gaos: Altair[IO],
                           tcs: TcsNorthAoConfig): IO[Unit] = {
    def configParams(current: EpicsTcsAoConfig): List[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] = List(
      setPwfs1Probe(EpicsTcsAoConfig.base)(subsystems, current.base.pwfs1.tracking, tcs.gds.pwfs1.tracking),
      setAltairProbe(subsystems, current.aowfs, tcs.gds.aoguide.tracking),
      setOiwfsProbe(EpicsTcsAoConfig.base)(subsystems, current.base.oiwfs.tracking, tcs.gds.oiwfs.tracking),
      tcs.tc.offsetA.flatMap(o => applyParam(subsystems.contains(Subsystem.Mount), current.base.offset,
        o.toFocalPlaneOffset(current.base.iaa), setTelescopeOffset, EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.offset
      )),
      tcs.tc.wavelA.flatMap(applyParam(subsystems.contains(Subsystem.Mount), current.base.wavelA, _, setWavelength,
        EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.wavelA
      )),
      setScienceFold(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc.sfPos),
      setHrPickup(EpicsTcsAoConfig.base)(subsystems, current, tcs.agc)
    ).mapFilter(identity)

    def sysConfig(current: EpicsTcsAoConfig): IO[EpicsTcsAoConfig] = {
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
      s0 <- TcsConfigRetriever.retrieveConfigurationNorth(gaos.isFollowing)
      _  <- if(s0.base.useAo) IO.unit
            else SeqexecFailure.Execution("Found useAo not set for AO step.").raiseError[IO, Unit]
      pr <- pauseResumeGaos[IO](gaos, s0, tcs)
      _  <- pr.pause.getOrElse(IO.unit)
      s1 <- guideOff(subsystems, s0, tcs, pr.pause.isEmpty)
      s2 <- sysConfig(s1)
      _  <- guideOn(subsystems, s2, tcs, pr.resume.isDefined)
      _  <- pr.resume.getOrElse(IO.unit) //resume Gaos
    } yield ()
  }

  def guideParams(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig)
  : List[EpicsTcsAoConfig => IO[EpicsTcsAoConfig]] = List(
    setMountGuide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.mountGuide, demand.gc.mountGuide),
    setM1Guide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.m1Guide, demand.gc.m1Guide),
    setM2Guide(EpicsTcsAoConfig.base)(subsystems, current.base.telescopeGuideConfig.m2Guide, demand.gc.m2Guide),
    setPwfs1(EpicsTcsAoConfig.base)(subsystems, current.base.pwfs1.detector, demand.gds.pwfs1.detector),
    setOiwfs(EpicsTcsAoConfig.base)(subsystems, current.base.oiwfs.detector, demand.gds.oiwfs.detector)
  ).mapFilter(identity)

  def tagIso[B, T]: Iso[B@@T, B] = Iso.apply[B@@T, B](x => x)(tag[T](_))

  def calcGuideOff(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig, gaosEnabled: Boolean): TcsNorthAoConfig = {
    val mustOff = mustPauseWhileOffsetting(current, demand)
    // Only turn things off here. Things that must be turned on will be turned on in GuideOn.
    def calc(c: GuiderSensorOption, d: GuiderSensorOption) = (mustOff || d === GuiderSensorOff).fold(GuiderSensorOff, c)

    (AoTcsConfig.gds[GuiderConfig@@AoGuide, AltairConfig].modify(
      (AoGuidersConfig.pwfs1[GuiderConfig@@AoGuide] ^<-> tagIso ^|-> GuiderConfig.detector)
        .set(calc(current.base.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
        (AoGuidersConfig.oiwfs[GuiderConfig@@AoGuide] ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.base.oiwfs.detector, demand.gds.oiwfs.detector))
    ) >>> AoTcsConfig.gc[GuiderConfig@@AoGuide, AltairConfig].modify(
      TelescopeGuideConfig.mountGuide.set(
        (mustOff || demand.gc.mountGuide === MountGuideOption.MountGuideOff).fold(MountGuideOption.MountGuideOff, current.base.telescopeGuideConfig.mountGuide)
      ) >>>
        TelescopeGuideConfig.m1Guide.set(
          (mustOff || demand.gc.m1Guide === M1GuideConfig.M1GuideOff).fold(M1GuideConfig.M1GuideOff, current.base.telescopeGuideConfig.m1Guide)
        ) >>>
        TelescopeGuideConfig.m2Guide.set(
          (mustOff || demand.gc.m2Guide === M2GuideConfig.M2GuideOff).fold(M2GuideConfig.M2GuideOff, current.base.telescopeGuideConfig.m2Guide)
        )
    ) >>> normalizeM1Guiding(gaosEnabled) >>> normalizeM2Guiding(gaosEnabled) >>> normalizeMountGuiding)(demand)
  }

  def calcAoPauseConditions(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig): Set[PauseCondition] = Set(
    demand.tc.offsetA.flatMap(v => (v =!= current.base.instrumentOffset)
      .option(PauseCondition.OffsetMove(current.base.offset, v.toFocalPlaneOffset(current.base.iaa)))),
    (current.base.oiwfs.detector === GuiderSensorOn && demand.gds.oiwfs.detector === GuiderSensorOff).option(PauseCondition.OiOff),
    (current.base.pwfs1.detector === GuiderSensorOn && demand.gds.pwfs1.detector === GuiderSensorOff).option(PauseCondition.P1Off),
    (demand.gds.aoguide.detector === GuiderSensorOff).option(PauseCondition.GaosGuideOff)
  ).collect{ case Some(x) => x }

  def calcAoResumeConditions(current: EpicsTcsAoConfig, demand: TcsNorthAoConfig): Set[ResumeCondition] = Set(
    demand.tc.offsetA.map(v => ResumeCondition.OffsetReached(v.toFocalPlaneOffset(current.base.iaa))),
    (demand.gds.oiwfs.detector === GuiderSensorOn).option(ResumeCondition.OiOn),
    (demand.gds.pwfs1.detector === GuiderSensorOn).option(ResumeCondition.P1On),
    (demand.gds.aoguide.detector === GuiderSensorOn).option(ResumeCondition.GaosGuideOn)
  ).collect{ case Some(x) => x }

  def pauseResumeGaos[F[_]: Sync ](gaos: Altair[F], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig)
  : F[PauseResume[F]] =
    gaos.pauseResume(demand.gaos, calcAoPauseConditions(current, demand), calcAoResumeConditions(current, demand))

  def updateEpicsGuideConfig(epicsCfg: EpicsTcsAoConfig, demand: TcsNorthAoConfig): EpicsTcsAoConfig = (
    (EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.telescopeGuideConfig).set(demand.gc) >>>
      (EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector).set(demand.gds.pwfs1.detector) >>>
      (EpicsTcsAoConfig.base ^|-> BaseEpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector).set(demand.gds.oiwfs.detector)
    )(epicsCfg)

  def guideOff(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig, pauseGaos: Boolean)
  : IO[EpicsTcsAoConfig] = {
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

  def guideOn(subsystems: NonEmptySet[Subsystem], current: EpicsTcsAoConfig, demand: TcsNorthAoConfig,
              gaosEnabled: Boolean): IO[EpicsTcsAoConfig] = {
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

  def guiderActive(c: GuiderConfig): Boolean = c.tracking match {
    case ProbeTrackingConfig.On(_) => c.detector === GuiderSensorOn
    case _                         => false
  }

  // Disable M1 guiding if source is off
  def normalizeM1Guiding(gaosEnabled: Boolean): Endo[TcsNorthAoConfig] = cfg =>
    (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
      case g @ M1GuideConfig.M1GuideOn(src) => src match {
        case M1Source.PWFS1 => if(guiderActive(cfg.gds.pwfs1)) g else M1GuideConfig.M1GuideOff
        case M1Source.OIWFS => if(guiderActive(cfg.gds.oiwfs)) g else M1GuideConfig.M1GuideOff
        case M1Source.GAOS  => if(cfg.gds.aoguide.detector === GuiderSensorOn && gaosEnabled) g
                               else M1GuideConfig.M1GuideOff
        case _              => g
      }
      case x                => x
    }(cfg)

  // Disable M2 sources if they are off, disable M2 guiding if all are off
  def normalizeM2Guiding(gaosEnabled: Boolean): Endo[TcsNorthAoConfig] = cfg =>
    (AoTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).modify{
      case M2GuideConfig.M2GuideOn(coma, srcs) =>
        val ss = srcs.filter{
          case TipTiltSource.PWFS1 => guiderActive(cfg.gds.pwfs1)
          case TipTiltSource.OIWFS => guiderActive(cfg.gds.oiwfs)
          case TipTiltSource.GAOS  => cfg.gds.aoguide.detector === GuiderSensorOn && gaosEnabled
          case _                   => true
        }
        if(ss.isEmpty) M2GuideConfig.M2GuideOff
        else M2GuideConfig.M2GuideOn((cfg.gc.m1Guide =!= M1GuideConfig.M1GuideOff).fold(coma, ComaOption.ComaOff), ss)
      case x                     => x
    }(cfg)

  // Disable Mount guiding if M2 guiding is disabled
  val normalizeMountGuiding: Endo[TcsNorthAoConfig] = cfg =>
    (AoTcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
      case (MountGuideOption.MountGuideOn, M2GuideConfig.M2GuideOn(_, _)) => MountGuideOption.MountGuideOn
      case _                                                              => MountGuideOption.MountGuideOff
    } }(cfg)

  @Lenses
  final case class EpicsTcsAoConfig(
    base: BaseEpicsTcsConfig,
    aowfs: ProbeTrackingConfig
  )

}
