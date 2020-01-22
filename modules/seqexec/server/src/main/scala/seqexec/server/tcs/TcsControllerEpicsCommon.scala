// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats._
import cats.data._
import cats.effect.{Async, Sync, Timer}
import cats.implicits._
import edu.gemini.spModel.core.Wavelength
import io.chrisdavenport.log4cats.Logger
import gem.enum.LightSinkName
import mouse.boolean._
import monocle.Lens
import seqexec.model.enum.{ComaOption, M1Source, MountGuideOption, TipTiltSource}
import seqexec.model.M2GuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.server.EpicsCodex.encode
import seqexec.server.tcs.TcsController._
import seqexec.server.{EpicsCommand, SeqexecFailure}
import squants.time.TimeConversions._
import java.util.concurrent.TimeUnit.SECONDS
import java.time.Duration

import scala.concurrent.duration.FiniteDuration

/**
 * Base implementation of an Epics TcsController
 * Type parameter BaseEpicsTcsConfig is the class used to hold the current configuration
 */
sealed trait TcsControllerEpicsCommon[F[_]] {

  def applyBasicConfig(
    subsystems: NonEmptySet[Subsystem],
    tcs: BasicTcsConfig
  ): F[Unit]

  def notifyObserveStart: F[Unit]

  def notifyObserveEnd: F[Unit]

  def nod(
    subsystems: NonEmptySet[Subsystem],
    offset: InstrumentOffset,
    guided: Boolean,
    tcs: BasicTcsConfig
  ): F[Unit]

  def setMountGuide[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem],
    c: MountGuideOption,
    d: MountGuideOption
  ): Option[C => F[C]]

  def setM1Guide[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem],
    c: M1GuideConfig,
    d: M1GuideConfig
  ): Option[C => F[C]]

  def setM2Guide[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem],
    c: M2GuideConfig,
    d: M2GuideConfig
  ): Option[C => F[C]]

  def setPwfs1[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem],
    c: GuiderSensorOption,
    d: GuiderSensorOption
  ): Option[C => F[C]]

  def setOiwfs[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem],
    c: GuiderSensorOption,
    d: GuiderSensorOption
  ): Option[C => F[C]]

  def setScienceFold[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem],
    c: C,
    d: LightPath
  ): Option[C => F[C]]

  def setHrPickup[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem],
    current: C,
    d: AGConfig
  ): Option[C => F[C]]

  def setTelescopeOffset(c: FocalPlaneOffset): F[Unit]

  def setWavelength(w: Wavelength): F[Unit]

  def setPwfs1Probe[C](l: Lens[C, BaseEpicsTcsConfig])(
    a: NonEmptySet[Subsystem],
    b: ProbeTrackingConfig,
    c: ProbeTrackingConfig
  ): Option[C => F[C]]

  def setOiwfsProbe[C](l: Lens[C, BaseEpicsTcsConfig])(
    a: NonEmptySet[Subsystem],
    b: ProbeTrackingConfig,
    c: ProbeTrackingConfig
  ): Option[C => F[C]]

  def setNodChopProbeTrackingConfig(s: TcsEpics.ProbeGuideCmd[F])(
    c: NodChopTrackingConfig
  ): F[Unit]

  def setGuideProbe[C](
    guideControl: GuideControl[F],
    trkSet: ProbeTrackingConfig => C => C)(
    subsystems: NonEmptySet[Subsystem],
    c: ProbeTrackingConfig,
    d: ProbeTrackingConfig
  ): Option[C => F[C]]
}

/*
 * Base implementation of an Epics TcsController
 * Type parameter BaseEpicsTcsConfig is the class used to hold the current configuration
 */
object TcsControllerEpicsCommon {
  private def mustPauseWhileOffsetting(current: BaseEpicsTcsConfig, demand: BasicTcsConfig): Boolean = {
    val distanceSquared = demand.tc.offsetA.map(_.toFocalPlaneOffset(current.iaa))
      .map { o => (o.x - current.offset.x, o.y - current.offset.y) }
      .map(d => d._1 * d._1 + d._2 * d._2)

    val thresholds = List(
      (Tcs.calcGuiderInUse(demand.gc, TipTiltSource.PWFS1, M1Source.PWFS1) && demand.gds.pwfs1.isActive)
        .option(pwfs1OffsetThreshold),
      (Tcs.calcGuiderInUse(demand.gc, TipTiltSource.PWFS2, M1Source.PWFS2) && demand.gds.pwfs2.isActive)
        .option(pwfs2OffsetThreshold),
      demand.inst.oiOffsetGuideThreshold
        .filter(_ => Tcs.calcGuiderInUse(demand.gc, TipTiltSource.OIWFS, M1Source.OIWFS) && demand.gds.oiwfs.isActive)
    )
    // Does the offset movement surpass any of the existing thresholds ?
    distanceSquared.exists(dd => thresholds.exists(_.exists(t => t*t < dd)))
  }

  // Disable M1 guiding if source is off
  private def normalizeM1Guiding(gaosEnabled: Boolean): Endo[BasicTcsConfig] = cfg =>
    (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
      case g @ M1GuideConfig.M1GuideOn(src) => src match {
        case M1Source.PWFS1 => if(cfg.gds.pwfs1.isActive) g else M1GuideConfig.M1GuideOff
        case M1Source.PWFS2 => if(cfg.gds.pwfs2.isActive) g else M1GuideConfig.M1GuideOff
        case M1Source.OIWFS => if(cfg.gds.oiwfs.isActive) g else M1GuideConfig.M1GuideOff
        case M1Source.GAOS  => if(gaosEnabled) g else M1GuideConfig.M1GuideOff
        case _              => g
      }
      case x                => x
    }(cfg)

  // Disable M2 sources if they are off, disable M2 guiding if all are off
  private def normalizeM2Guiding(gaosEnabled: Boolean): Endo[BasicTcsConfig] = cfg =>
    (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).modify{
      case M2GuideConfig.M2GuideOn(coma, srcs) =>
        val ss = srcs.filter{
          case TipTiltSource.PWFS1 => cfg.gds.pwfs1.isActive
          case TipTiltSource.PWFS2 => cfg.gds.pwfs2.isActive
          case TipTiltSource.OIWFS => cfg.gds.oiwfs.isActive
          case TipTiltSource.GAOS  => gaosEnabled
          case _                   => true
        }
        if(ss.isEmpty) M2GuideConfig.M2GuideOff
        else M2GuideConfig.M2GuideOn((cfg.gc.m1Guide =!= M1GuideConfig.M1GuideOff).fold(coma, ComaOption.ComaOff), ss)
      case x                     => x
    }(cfg)

  // Disable Mount guiding if M2 guiding is disabled
  private val normalizeMountGuiding: Endo[BasicTcsConfig] = cfg =>
    (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
      case (MountGuideOption.MountGuideOn, M2GuideConfig.M2GuideOn(_, _)) => MountGuideOption.MountGuideOn
      case _                                                              => MountGuideOption.MountGuideOff
    } }(cfg)

  private def calcGuideOff(current: BaseEpicsTcsConfig, demand: BasicTcsConfig): BasicTcsConfig = {
    val mustOff = mustPauseWhileOffsetting(current, demand)
    // Only turn things off here. Things that must be turned on will be turned on in GuideOn.
    def calc(c: GuiderSensorOption, d: GuiderSensorOption) = (mustOff || d === GuiderSensorOff).fold(GuiderSensorOff, c)

    (BasicTcsConfig.gds.modify(
      (BasicGuidersConfig.pwfs1 ^<-> tagIso ^|-> GuiderConfig.detector)
        .set(calc(current.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
        (BasicGuidersConfig.pwfs2 ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.pwfs2.detector, demand.gds.pwfs2.detector)) >>>
        (BasicGuidersConfig.oiwfs ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.oiwfs.detector, demand.gds.oiwfs.detector))
    ) >>> BasicTcsConfig.gc.modify(
      TelescopeGuideConfig.mountGuide.set(
        (mustOff || demand.gc.mountGuide === MountGuideOption.MountGuideOff).fold(MountGuideOption.MountGuideOff, current.telescopeGuideConfig.mountGuide)
      ) >>>
        TelescopeGuideConfig.m1Guide.set(
          (mustOff || demand.gc.m1Guide === M1GuideConfig.M1GuideOff).fold(M1GuideConfig.M1GuideOff, current.telescopeGuideConfig.m1Guide)
        ) >>>
        TelescopeGuideConfig.m2Guide.set(
          (mustOff || demand.gc.m2Guide === M2GuideConfig.M2GuideOff).fold(M2GuideConfig.M2GuideOff, current.telescopeGuideConfig.m2Guide)
        )
    ) >>> normalizeM1Guiding(false) >>> normalizeM2Guiding(false) >>> normalizeMountGuiding)(demand)
  }

  def applyParam[F[_]: Applicative, T: Eq, C](
    used: Boolean,
    current: T,
    demand: T,
    act: T => F[Unit],
    lens: Lens[C, T]
  ): Option[C => F[C]] =
    (used && current =!= demand).option(c => act(demand) *> lens.set(demand)(c).pure[F])

  private class TcsControllerEpicsCommonImpl[F[_]: Async: Timer](epicsSys: TcsEpics[F])(implicit L: Logger[F]) extends TcsControllerEpicsCommon[F] with TcsControllerEncoders with ScienceFoldPositionCodex {
    private val tcsConfigRetriever = TcsConfigRetriever[F](epicsSys)

    override def setMountGuide[C](l: Lens[C, BaseEpicsTcsConfig])(
      subsystems: NonEmptySet[Subsystem], c: MountGuideOption, d: MountGuideOption
    ): Option[C => F[C]] = applyParam(subsystems.contains(Subsystem.Mount), c, d,
      (x: MountGuideOption) => epicsSys.mountGuideCmd.setMode(encode(x)),
      l ^|-> BaseEpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.mountGuide
    )

    override def setM1Guide[C](l: Lens[C, BaseEpicsTcsConfig])(
      subsystems: NonEmptySet[Subsystem], c: M1GuideConfig, d: M1GuideConfig
    ): Option[C => F[C]] = applyParam(subsystems.contains(Subsystem.M1), c, d,
      (x: M1GuideConfig) => epicsSys.m1GuideCmd.setState(encode(x)),
      l ^|-> BaseEpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m1Guide
    )

    override def setM2Guide[C](l: Lens[C, BaseEpicsTcsConfig])(
      subsystems: NonEmptySet[Subsystem], c: M2GuideConfig, d: M2GuideConfig
    ): Option[C => F[C]] = {
      val actions = List(
        epicsSys.m2GuideModeCmd.setComa(encodeM2Coma.encode(d))
          .whenA(encodeM2Coma.encode(d) =!= encodeM2Coma.encode(c)),
        epicsSys.m2GuideCmd.setState(encodeM2Guide.encode(d))
          .whenA(encodeM2Guide.encode(d) =!= encodeM2Guide.encode(c)),
        epicsSys.m2GuideConfigCmd.setReset(encodeM2GuideReset.encode(d))
          .whenA(encodeM2GuideReset.encode(d) =!= encodeM2GuideReset.encode(c))
      )

      (subsystems.contains(Subsystem.M2) && actions.nonEmpty).option( x =>
        actions.sequence *> Sync[F].delay(
          (l ^|-> BaseEpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m2Guide).set(d)(x)
        )
      )
    }

    val NonStopExposures: Int = -1

    private def setGuiderWfs(on: TcsEpics.WfsObserveCmd[F], off: EpicsCommand[F])(c: GuiderSensorOption)
    : F[Unit] =
      c match {
        case GuiderSensorOff => off.mark
        case GuiderSensorOn  => on.setNoexp(NonStopExposures) // Set number of exposures to non-stop (-1)
      }

    override def setPwfs1[C](l: Lens[C, BaseEpicsTcsConfig])(
      subsystems: NonEmptySet[Subsystem], c: GuiderSensorOption, d: GuiderSensorOption
    ): Option[C => F[C]] = applyParam(subsystems.contains(Subsystem.PWFS1), c, d,
      setGuiderWfs(epicsSys.pwfs1ObserveCmd, epicsSys.pwfs1StopObserveCmd)(_: GuiderSensorOption),
      l ^|-> BaseEpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector
    )

    private def setPwfs2[C](l: Lens[C, BaseEpicsTcsConfig])(
      subsystems: NonEmptySet[Subsystem], c: GuiderSensorOption, d: GuiderSensorOption
    ): Option[C => F[C]] = applyParam(subsystems.contains(Subsystem.PWFS2), c, d,
      setGuiderWfs(epicsSys.pwfs2ObserveCmd, epicsSys.pwfs2StopObserveCmd)(_: GuiderSensorOption),
      l ^|-> BaseEpicsTcsConfig.pwfs2 ^|-> GuiderConfig.detector
    )

    override def setOiwfs[C](l: Lens[C, BaseEpicsTcsConfig])(
      subsystems: NonEmptySet[Subsystem], c: GuiderSensorOption, d: GuiderSensorOption
    ): Option[C => F[C]] = applyParam(subsystems.contains(Subsystem.OIWFS), c, d,
      setGuiderWfs(epicsSys.oiwfsObserveCmd, epicsSys.oiwfsStopObserveCmd)(_: GuiderSensorOption),
      l ^|-> BaseEpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector
    )

    private def guideParams(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, demand: BasicTcsConfig)
    : List[BaseEpicsTcsConfig => F[BaseEpicsTcsConfig]] = List(
      setMountGuide(Lens.id)(subsystems, current.telescopeGuideConfig.mountGuide, demand.gc.mountGuide),
      setM1Guide(Lens.id)(subsystems, current.telescopeGuideConfig.m1Guide, demand.gc.m1Guide),
      setM2Guide(Lens.id)(subsystems, current.telescopeGuideConfig.m2Guide, demand.gc.m2Guide)
    ).flattenOption

    def setHRPickupConfig(hrwfsPos: HrwfsPickupPosition): F[Unit] = hrwfsPos match {
      case HrwfsPickupPosition.Parked => epicsSys.hrwfsParkCmd.mark
      case _                          => epicsSys.hrwfsPosCmd.setHrwfsPos(encode(hrwfsPos))
    }

    private def calcHrPickupPosition(c: AGConfig, ports: InstrumentPorts): Option[HrwfsPickupPosition] = c.hrwfs.flatMap {
      case HrwfsConfig.Manual(h) => h.some
      case HrwfsConfig.Auto      => scienceFoldFromRequested(ports)(c.sfPos).flatMap {
        case ScienceFold.Parked |
             ScienceFold.Position(_, _, BottomPort) => HrwfsPickupPosition.Parked.some
        case ScienceFold.Position(_, _, _)          => none
      }
    }

    override def setScienceFold[C](l: Lens[C, BaseEpicsTcsConfig])(subsystems: NonEmptySet[Subsystem], c: C, d: LightPath)
    : Option[C => F[C]] = {
      val base = l.get(c)
      scienceFoldFromRequested(base.instPorts)(d).flatMap { sf =>
        (subsystems.contains(Subsystem.AGUnit) && base.scienceFoldPosition.forall(_ =!= sf)).option { x =>
          setScienceFoldConfig(sf) *> Sync[F].delay((l ^|-> BaseEpicsTcsConfig.scienceFoldPosition).set(sf.some)(x))
        }
      }
    }

    /**
     * Positions Parked and OUT are equivalent for practical purposes. Therefore, if the current position is Parked and
     * requested position is OUT (or the other way around), then it is not necessary to move the HR pickup mirror.
     */
    override def setHrPickup[C](l: Lens[C, BaseEpicsTcsConfig])(
      subsystems: NonEmptySet[Subsystem], current: C, d: AGConfig
    ): Option[C => F[C]] = {
      val base = l.get(current)
      subsystems.contains(Subsystem.AGUnit).fold(
        calcHrPickupPosition(d, base.instPorts).flatMap { a =>
          val b = base.hrwfsPickupPosition
          (a =!= b && (HrwfsPickupPosition.isInTheWay(a) || HrwfsPickupPosition.isInTheWay(b))).option { x =>
            setHRPickupConfig(a) *> Sync[F].delay((l ^|-> BaseEpicsTcsConfig.hrwfsPickupPosition).set(a)(x))
          }
        },
        none
      )
    }

    private def guideOff(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, demand: BasicTcsConfig): F[BaseEpicsTcsConfig] = {
      val params = guideParams(subsystems, current, calcGuideOff(current, demand) )

      if(params.nonEmpty)
        for {
          s <- params.foldLeft(current.pure[F]){ case (c, p) => c.flatMap(p)}
          _ <- epicsSys.post(DefaultTimeout)
          _ <- L.debug("Turning guide off")
        } yield s
      else
        L.debug("Skipping guide off") *> current.pure[F]
    }

    override def setNodChopProbeTrackingConfig(s: TcsEpics.ProbeGuideCmd[F])(c: NodChopTrackingConfig): F[Unit] =
      s.setNodachopa(encode(c.get(NodChop(Beam.A, Beam.A)))) *>
        s.setNodachopb(encode(c.get(NodChop(Beam.A, Beam.B)))) *>
        s.setNodbchopa(encode(c.get(NodChop(Beam.B, Beam.A)))) *>
        s.setNodbchopb(encode(c.get(NodChop(Beam.B, Beam.B))))

    private def portFromSinkName(ports: InstrumentPorts)(n: LightSinkName): Option[Int] = {
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

    override def setGuideProbe[C](
      guideControl: GuideControl[F],
      trkSet: ProbeTrackingConfig => C => C)(
      subsystems: NonEmptySet[Subsystem],
      c: ProbeTrackingConfig,
      d: ProbeTrackingConfig): Option[C => F[C]] =
      if (subsystems.contains(guideControl.subs)) {
        val actions = List(
          (c.getNodChop =!= d.getNodChop)
            .option(setNodChopProbeTrackingConfig(guideControl.nodChopGuideCmd)(d.getNodChop)),
          d match {
            case ProbeTrackingConfig.Parked => (c =!= ProbeTrackingConfig.Parked).option(guideControl.parkCmd.mark)
            case ProbeTrackingConfig.On(_) |
                 ProbeTrackingConfig.Off |
                 ProbeTrackingConfig.Frozen => (c.follow =!= d.follow)
              .option(guideControl.followCmd.setFollowState(encode(d.follow)))
          }
        ).flattenOption

        actions.nonEmpty.option { x =>
          actions.sequence *>
            trkSet(d)(x).pure[F]
        }
      }
      else none

    private def pwfs1GuiderControl: GuideControl[F] =
      GuideControl(Subsystem.PWFS1, epicsSys.pwfs1Park,
        epicsSys.pwfs1ProbeGuideCmd, epicsSys.pwfs1ProbeFollowCmd)

    override def setPwfs1Probe[C](l: Lens[C, BaseEpicsTcsConfig])(
      a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
    ): Option[C => F[C]] =
      setGuideProbe(pwfs1GuiderControl, (l ^|-> BaseEpicsTcsConfig.pwfs1 ^|-> GuiderConfig.tracking).set)(a, b, c)

    private def pwfs2GuiderControl: GuideControl[F] =
      GuideControl(Subsystem.PWFS2, epicsSys.pwfs2Park,
        epicsSys.pwfs2ProbeGuideCmd, epicsSys.pwfs2ProbeFollowCmd)

    def setPwfs2Probe[C](l: Lens[C, BaseEpicsTcsConfig])(
      a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
    ): Option[C => F[C]] =
      setGuideProbe(pwfs2GuiderControl, (l ^|-> BaseEpicsTcsConfig.pwfs2 ^|-> GuiderConfig.tracking).set)(a, b, c)

    private def oiwfsGuiderControl: GuideControl[F] =
      GuideControl(Subsystem.OIWFS, epicsSys.oiwfsPark,
        epicsSys.oiwfsProbeGuideCmd, epicsSys.oiwfsProbeFollowCmd)

    override def setOiwfsProbe[C](l: Lens[C, BaseEpicsTcsConfig])(
      a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
    ): Option[C => F[C]] =
      setGuideProbe(oiwfsGuiderControl, (l ^|-> BaseEpicsTcsConfig.oiwfs ^|-> GuiderConfig.tracking).set)(a, b, c)

    // Same offset is applied to all the beams
    override def setTelescopeOffset(c: FocalPlaneOffset): F[Unit] =
      epicsSys.offsetACmd.setX(c.x.toMillimeters) *>
        epicsSys.offsetACmd.setY(c.y.toMillimeters) *>
        epicsSys.offsetBCmd.setX(c.x.toMillimeters) *>
        epicsSys.offsetBCmd.setY(c.y.toMillimeters)

    // Same wavelength is applied to all the beams
    override def setWavelength(w: Wavelength): F[Unit] =
      epicsSys.wavelSourceA.setWavel(w.toMicrons) *>
        epicsSys.wavelSourceB.setWavel(w.toMicrons)

    def scienceFoldFromRequested(ports: InstrumentPorts)(r: LightPath): Option[ScienceFold] =
      portFromSinkName(ports)(r.sink).map { p =>
        if (p === BottomPort && r.source === LightSource.Sky) ScienceFold.Parked
        else ScienceFold.Position(r.source, r.sink, p)
      }

    def setScienceFoldConfig(sfPos: ScienceFold): F[Unit] = sfPos match {
      case ScienceFold.Parked => epicsSys.scienceFoldParkCmd.mark
      case p: ScienceFold.Position => epicsSys.scienceFoldPosCmd.setScfold(encode(p))
    }

    private def configBaseParams(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, tcs: BasicTcsConfig)
    : List[BaseEpicsTcsConfig => F[BaseEpicsTcsConfig]] = List(
      setPwfs1Probe(Lens.id)(subsystems, current.pwfs1.tracking, tcs.gds.pwfs1.tracking),
      setPwfs2Probe(Lens.id)(subsystems, current.pwfs2.tracking, tcs.gds.pwfs2.tracking),
      setOiwfsProbe(Lens.id)(subsystems, current.oiwfs.tracking, tcs.gds.oiwfs.tracking),
      setPwfs1(Lens.id)(subsystems, current.pwfs1.detector, tcs.gds.pwfs1.detector),
      setPwfs2(Lens.id)(subsystems, current.pwfs2.detector, tcs.gds.pwfs2.detector),
      setOiwfs(Lens.id)(subsystems, current.oiwfs.detector, tcs.gds.oiwfs.detector),
      tcs.tc.offsetA.flatMap(o => applyParam(subsystems.contains(Subsystem.Mount), current.offset,
        o.toFocalPlaneOffset(current.iaa), setTelescopeOffset, BaseEpicsTcsConfig.offset
      )),
      tcs.tc.wavelA.flatMap(applyParam(subsystems.contains(Subsystem.Mount), current.wavelA, _, setWavelength,
        BaseEpicsTcsConfig.wavelA
      )),
      setScienceFold(Lens.id)(subsystems, current, tcs.agc.sfPos),
      setHrPickup(Lens.id)(subsystems, current, tcs.agc)
    ).flattenOption

  def guideOn(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, demand: BasicTcsConfig)
  : F[BaseEpicsTcsConfig] = {

    // If the demand turned off any WFS, normalize will turn off the corresponding processing
    val normalizedGuiding = (normalizeM1Guiding(false) >>> normalizeM2Guiding(false) >>>
      normalizeMountGuiding)(demand)

    val params = guideParams(subsystems, current, normalizedGuiding)

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(Sync[F].delay(current)){ case (c, p) => c.flatMap(p)}
        _ <- epicsSys.post(DefaultTimeout)
        _ <- L.debug("Turning guide on")
      } yield s
    else
      L.debug("Skipping guide on") *> Sync[F].delay(current)
  }


    override def applyBasicConfig(subsystems: NonEmptySet[Subsystem], tcs: BasicTcsConfig): F[Unit] = {
      def sysConfig(current: BaseEpicsTcsConfig): F[BaseEpicsTcsConfig] = {
        val params = configBaseParams(subsystems, current, tcs)
        val stabilizationTime = tcs.tc.offsetA
          .map(TcsSettleTimeCalculator.calc(current.instrumentOffset, _, subsystems, tcs.inst.instrument))
          .getOrElse(0.seconds)

        if(params.nonEmpty)
          for {
            _ <- L.debug("Start TCS configuration")
            s <- params.foldLeft(current.pure[F]){ case (c, p) => c.flatMap(p) }
            _ <- epicsSys.post(ConfigTimeout)
            _ <- if(subsystems.contains(Subsystem.Mount))
              epicsSys.waitInPosition(Duration.ofMillis(stabilizationTime.toMillis), tcsTimeout) *> L.debug("TCS inposition")
            else if(Set(Subsystem.PWFS1, Subsystem.PWFS2, Subsystem.AGUnit).exists(subsystems.contains))
              epicsSys.waitAGInPosition(agTimeout) *> L.debug("AG inposition")
            else Sync[F].unit
            _ <- L.debug("Completed TCS configuration")
          } yield s
        else
          L.debug("Skipping TCS configuration") *> current.pure[F]
      }

      for {
        s0 <- tcsConfigRetriever.retrieveBaseConfiguration
        _  <- SeqexecFailure.Execution("Found useAo set for non AO step.").raiseError[F, Unit].whenA(s0.useAo)
        s1 <- guideOff(subsystems, s0, tcs)
        s2 <- sysConfig(s1)
        _  <- guideOn(subsystems, s2, tcs)
      } yield ()
    }
    override def notifyObserveStart: F[Unit] =
      L.debug("Send observe to TCS") *>
        epicsSys.observe.mark *>
        epicsSys.post(DefaultTimeout).void

    override def notifyObserveEnd: F[Unit] =
      L.debug("Send endObserve to TCS") *>
        epicsSys.endObserve.mark *>
        epicsSys.post(DefaultTimeout).void

    // To nod the telescope is just like applying a TCS configuration, but always with an offset
    override def nod(subsystems: NonEmptySet[Subsystem], offset: InstrumentOffset, guided: Boolean, tcs: BasicTcsConfig): F[Unit] = {

      val offsetConfig: BasicTcsConfig = (BasicTcsConfig.tc ^|-> TelescopeConfig.offsetA).set(offset.some)(tcs)
      val noddedConfig: BasicTcsConfig =
        if(guided) offsetConfig
        else BasicTcsConfig.gds.modify(
          (BasicGuidersConfig.pwfs1 ^<-> tagIso).modify(
            GuiderConfig.tracking.modify{ tr => if(tr.isActive) ProbeTrackingConfig.Frozen else tr} >>>
            GuiderConfig.detector.set(GuiderSensorOff)
          ) >>>
            (BasicGuidersConfig.pwfs2 ^<-> tagIso).modify(
              GuiderConfig.tracking.modify{ tr => if(tr.isActive) ProbeTrackingConfig.Frozen else tr} >>>
              GuiderConfig.detector.set(GuiderSensorOff)
            ) >>>
            (BasicGuidersConfig.oiwfs ^<-> tagIso).modify(
              GuiderConfig.tracking.modify{ tr => if(tr.isActive) ProbeTrackingConfig.Frozen else tr} >>>
              GuiderConfig.detector.set(GuiderSensorOff)
            )
        )(offsetConfig)

      applyBasicConfig(subsystems, noddedConfig)
    }

  }

  def apply[F[_]: Async: Logger: Timer](epicsSys: TcsEpics[F]): TcsControllerEpicsCommon[F] =
    new TcsControllerEpicsCommonImpl(epicsSys)

  val DefaultTimeout : FiniteDuration = FiniteDuration(10, SECONDS)
  val ConfigTimeout : FiniteDuration = FiniteDuration(60, SECONDS)

}
