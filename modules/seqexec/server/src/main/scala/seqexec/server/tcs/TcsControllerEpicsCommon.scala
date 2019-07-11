// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data._
import cats.effect.{Async, IO, Sync}
import cats.implicits._
import edu.gemini.spModel.core.Wavelength
import gem.enum.LightSinkName
import squants.time.Seconds
import mouse.boolean._
import cats.{Applicative, Endo, Eq}
import monocle.macros.Lenses
import monocle.{Iso, Lens}
import org.log4s.{Logger, getLogger}
import squants.{Angle, Length, Time}
import seqexec.model.enum.{ComaOption, M1Source, MountGuideOption, TipTiltSource}
import seqexec.model.M2GuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.server.EpicsCodex.{EncodeEpicsValue, encode}
import seqexec.server.tcs.TcsController._
import seqexec.server.{EpicsCommandF, SeqexecFailure}
import seqexec.server.tcs.TcsEpics.{ProbeFollowCmd, ProbeGuideCmd}
import seqexec.server.tcs.ScienceFoldPositionCodex._
import shapeless.tag
import shapeless.tag.@@
import squants.space.Arcseconds

/*
 * Base implementation of an Epics TcsController
 * Type parameter BaseEpicsTcsConfig is the class used to hold the current configuration
 */
object TcsControllerEpicsCommon {

  val logger: Logger = getLogger

  def applyBasicConfig(subsystems: NonEmptySet[Subsystem],
                           tcs: BasicTcsConfig): IO[Unit] = {

    def sysConfig(current: BaseEpicsTcsConfig): IO[BaseEpicsTcsConfig] = {
      val params = configBaseParams(subsystems, current, tcs)

      if(params.nonEmpty)
        for {
          s <- params.foldLeft(IO(current)){ case (c, p) => c.flatMap(p) }
          _ <- TcsEpics.instance.post
          _ <- IO(logger.debug("TCS configuration command post"))
          _ <- if(subsystems.contains(Subsystem.Mount))
            TcsEpics.instance.waitInPosition(tcsTimeout) *> IO.apply(logger.info("TCS inposition"))
          else if(Set(Subsystem.PWFS1, Subsystem.PWFS2, Subsystem.AGUnit).exists(subsystems.contains))
            TcsEpics.instance.waitAGInPosition(agTimeout) *> IO.apply(logger.debug("AG inposition"))
          else IO.unit
        } yield s
      else
        IO(logger.debug("Skipping TCS configuration")) *> IO(current)
    }

    for {
      s0 <- TcsConfigRetriever.retrieveBaseConfiguration
      _  <- if(s0.useAo) SeqexecFailure.Execution("Found useAo set for non AO step.").raiseError[IO, Unit] else IO.unit
      s1 <- guideOff(subsystems, s0, tcs)
      s2 <- sysConfig(s1)
      _  <- guideOn(subsystems, s2, tcs)
    } yield ()
  }

  def guideOff(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, demand: BasicTcsConfig)
  : IO[BaseEpicsTcsConfig] = {
    val params = guideParams(subsystems, current, calcGuideOff(current, demand) )

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(IO(current)){ case (c, p) => c.flatMap(p)}
        _ <- TcsEpics.instance.post
        _ <- IO(logger.info("Turning guide off"))
      } yield s
    else
      IO(logger.info("Skipping guide off")) *> IO(current)
  }

  def guideOn(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, demand: BasicTcsConfig)
  : IO[BaseEpicsTcsConfig] = {

    // If the demand turned off any WFS, normalize will turn off the corresponding processing
    val normalizedGuiding = (normalizeM1Guiding(false) >>> normalizeM2Guiding(false) >>>
      normalizeMountGuiding)(demand)

    val params = guideParams(subsystems, current, normalizedGuiding)

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(IO(current)){ case (c, p) => c.flatMap(p)}
        _ <- TcsEpics.instance.post
        _ <- IO(logger.info("Turning guide on"))
      } yield s
    else
      IO(logger.info("Skipping guide on")) *> IO(current)
  }

  def tagIso[B, T]: Iso[B@@T, B] = Iso.apply[B@@T, B](x => x)(tag[T](_))

  def notifyObserveStart: IO[Unit] =
    TcsEpics.instance.observe.mark[IO] *> TcsEpics.instance.post.void

  def notifyObserveEnd: IO[Unit] = TcsEpics.instance.endObserve.mark[IO] *> TcsEpics.instance.post.void

  def guiderActive(c: GuiderConfig): Boolean = c.tracking match {
    case ProbeTrackingConfig.On(_) => c.detector === GuiderSensorOn
    case _                         => false
  }

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

  val BottomPort: Int = 1
  val InvalidPort: Int = 0

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

  final case class GuideControl[F[_]: Async](subs: Subsystem,
                                             parkCmd: EpicsCommandF,
                                             nodChopGuideCmd: ProbeGuideCmd[F],
                                             followCmd: ProbeFollowCmd[F]
                                            )

  val pwfs1OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE
  val pwfs2OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE

  @Lenses
  final case class BaseEpicsTcsConfig(
    iaa: Angle,
    offset: FocalPlaneOffset,
    wavelA: Wavelength,
    pwfs1: GuiderConfig,
    pwfs2: GuiderConfig,
    oiwfs: GuiderConfig,
    telescopeGuideConfig: TelescopeGuideConfig,
    aoFold: AoFold,
    useAo: Boolean,
    scienceFoldPosition: Option[ScienceFold],
    hrwfsPickupPosition: HrwfsPickupPosition,
    instPorts: InstrumentPorts
  ) {
    val instrumentOffset: InstrumentOffset = offset.toInstrumentOffset(iaa)
  }

  // Same offset is applied to all the beams
  def setTelescopeOffset(c: FocalPlaneOffset): IO[Unit] =
    TcsEpics.instance.offsetACmd.setX(c.x.toMillimeters) *>
      TcsEpics.instance.offsetACmd.setY(c.y.toMillimeters) *>
      TcsEpics.instance.offsetBCmd.setX(c.x.toMillimeters) *>
      TcsEpics.instance.offsetBCmd.setY(c.y.toMillimeters) *>
      TcsEpics.instance.offsetCCmd.setX(c.x.toMillimeters) *>
      TcsEpics.instance.offsetCCmd.setY(c.y.toMillimeters)

  // Same wavelength is applied to all the beams
  def setWavelength(w: Wavelength): IO[Unit] =
    TcsEpics.instance.wavelSourceA.setWavel(w.toMicrons) *>
      TcsEpics.instance.wavelSourceB.setWavel(w.toMicrons) *>
      TcsEpics.instance.wavelSourceC.setWavel(w.toMicrons)

  implicit val encodeNodChopOption: EncodeEpicsValue[NodChopTrackingOption, String] =
    EncodeEpicsValue {
      case NodChopTrackingOption.NodChopTrackingOn => "On"
      case NodChopTrackingOption.NodChopTrackingOff => "Off"
    }

  implicit val encodeFollowOption: EncodeEpicsValue[FollowOption, String] =
    EncodeEpicsValue {
      case FollowOption.FollowOn => "On"
      case FollowOption.FollowOff => "Off"
    }

  def setNodChopProbeTrackingConfig[F[_] : Async](s: TcsEpics.ProbeGuideCmd[F])(c: NodChopTrackingConfig): F[Unit] =
    s.setNodachopa(encode(c.get(NodChop(Beam.A, Beam.A)))) *>
      s.setNodachopb(encode(c.get(NodChop(Beam.A, Beam.B)))) *>
      s.setNodachopc(encode(c.get(NodChop(Beam.A, Beam.C)))) *>
      s.setNodbchopa(encode(c.get(NodChop(Beam.B, Beam.A)))) *>
      s.setNodbchopb(encode(c.get(NodChop(Beam.B, Beam.B)))) *>
      s.setNodbchopc(encode(c.get(NodChop(Beam.B, Beam.C)))) *>
      s.setNodcchopa(encode(c.get(NodChop(Beam.C, Beam.A)))) *>
      s.setNodcchopb(encode(c.get(NodChop(Beam.C, Beam.B)))) *>
      s.setNodcchopc(encode(c.get(NodChop(Beam.C, Beam.C))))

  private def setGuideProbe[F[_] : Async, C](guideControl: GuideControl[F],
                                          trkSet: ProbeTrackingConfig => C => C)(
                                           subsystems: NonEmptySet[Subsystem], c: ProbeTrackingConfig, d: ProbeTrackingConfig
                                         ): Option[C => F[C]] =
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
      ).mapFilter(identity)

      actions.nonEmpty.option { x =>
        actions.sequence *>
          trkSet(d)(x).pure[F]
      }
    }
    else none

  private def pwfs1GuiderControl: GuideControl[IO] = GuideControl(Subsystem.PWFS1, TcsEpics.instance.pwfs1Park,
    TcsEpics.instance.pwfs1ProbeGuideCmd, TcsEpics.instance.pwfs1ProbeFollowCmd)

  def setPwfs1Probe[C](l: Lens[C, BaseEpicsTcsConfig])(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[C => IO[C]] =
    setGuideProbe(pwfs1GuiderControl, (l ^|-> BaseEpicsTcsConfig.pwfs1 ^|-> GuiderConfig.tracking).set)(a, b, c)

  private def pwfs2GuiderControl: GuideControl[IO] = GuideControl(Subsystem.PWFS2, TcsEpics.instance.pwfs2Park,
    TcsEpics.instance.pwfs2ProbeGuideCmd, TcsEpics.instance.pwfs2ProbeFollowCmd)

  def setPwfs2Probe[C](l: Lens[C, BaseEpicsTcsConfig])(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[C => IO[C]] =
    setGuideProbe(pwfs2GuiderControl, (l ^|-> BaseEpicsTcsConfig.pwfs2 ^|-> GuiderConfig.tracking).set)(a, b, c)

  private def oiwfsGuiderControl: GuideControl[IO] = GuideControl(Subsystem.OIWFS, TcsEpics.instance.oiwfsPark,
    TcsEpics.instance.oiwfsProbeGuideCmd, TcsEpics.instance.oiwfsProbeFollowCmd)

  def setOiwfsProbe[C](l: Lens[C, BaseEpicsTcsConfig])(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[C => IO[C]] =
    setGuideProbe(oiwfsGuiderControl, (l ^|-> BaseEpicsTcsConfig.oiwfs ^|-> GuiderConfig.tracking).set)(a, b, c)

  private def setGuiderWfs[F[_] : Sync](on: TcsEpics.WfsObserveCmd[F], off: EpicsCommandF)(c: GuiderSensorOption)
  : F[Unit] = {
    val NonStopExposures = -1
    c match {
      case GuiderSensorOff => off.mark
      case GuiderSensorOn => on.setNoexp(NonStopExposures) // Set number of exposures to non-stop (-1)
    }
  }

  def setPwfs1[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem], c: GuiderSensorOption, d: GuiderSensorOption
  ): Option[C => IO[C]] = applyParam(subsystems.contains(Subsystem.PWFS1), c, d,
    setGuiderWfs(TcsEpics.instance.pwfs1ObserveCmd, TcsEpics.instance.pwfs1StopObserveCmd)(_: GuiderSensorOption),
    l ^|-> BaseEpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector
  )

  def setPwfs2[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem], c: GuiderSensorOption, d: GuiderSensorOption
  ): Option[C => IO[C]] = applyParam(subsystems.contains(Subsystem.PWFS2), c, d,
    setGuiderWfs(TcsEpics.instance.pwfs2ObserveCmd, TcsEpics.instance.pwfs2StopObserveCmd)(_: GuiderSensorOption),
    l ^|-> BaseEpicsTcsConfig.pwfs2 ^|-> GuiderConfig.detector
  )

  def setOiwfs[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem], c: GuiderSensorOption, d: GuiderSensorOption
  ): Option[C => IO[C]] = applyParam(subsystems.contains(Subsystem.OIWFS), c, d,
    setGuiderWfs(TcsEpics.instance.oiwfsObserveCmd, TcsEpics.instance.oiwfsStopObserveCmd)(_: GuiderSensorOption),
    l ^|-> BaseEpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector
  )

  def portFromSinkName(ports: InstrumentPorts)(n: LightSinkName): Option[Int] = {
    import LightSinkName._
    val port = n match {
      case Gmos |
           Gmos_Ifu => ports.gmosPort
      case Niri_f6 |
           Niri_f14 |
           Niri_f32 => ports.niriPort
      case Nifs => ports.nifsPort
      case Gnirs => ports.gnirsPort
      case F2 => ports.flamingos2Port
      case Gpi => ports.gpiPort
      case Ghost => ports.ghostPort
      case Gsaoi => ports.gsaoiPort
      case Ac |
           Hr => BottomPort
      case Phoenix |
           Visitor => InvalidPort
    }
    (port =!= InvalidPort).option(port)
  }

  def scienceFoldFromRequested(ports: InstrumentPorts)(r: LightPath): Option[ScienceFold] =
    portFromSinkName(ports)(r.sink).map { p =>
      if (p === BottomPort && r.source === LightSource.Sky) ScienceFold.Parked
      else ScienceFold.Position(r.source, r.sink, p)
    }

  def setScienceFoldConfig(sfPos: ScienceFold): IO[Unit] = sfPos match {
    case ScienceFold.Parked => TcsEpics.instance.scienceFoldParkCmd.mark[IO]
    case p: ScienceFold.Position => TcsEpics.instance.scienceFoldPosCmd.setScfold(encode(p))
  }

  implicit private val encodeHrwfsPickupPosition: EncodeEpicsValue[HrwfsPickupPosition, String] =
    EncodeEpicsValue {
      case HrwfsPickupPosition.IN => "IN"
      case HrwfsPickupPosition.OUT => "OUT"
      case HrwfsPickupPosition.Parked => "park-pos."
    }

  def setHRPickupConfig(hrwfsPos: HrwfsPickupPosition): IO[Unit] = hrwfsPos match {
    case HrwfsPickupPosition.Parked => TcsEpics.instance.hrwfsParkCmd.mark[IO]
    case _ => TcsEpics.instance.hrwfsPosCmd.setHrwfsPos(encode(hrwfsPos))
  }

  private def calcHrPickupPosition(c: AGConfig, ports: InstrumentPorts): Option[HrwfsPickupPosition] = c.hrwfs.flatMap {
    case HrwfsConfig.Manual(h) => h.some
    case HrwfsConfig.Auto => scienceFoldFromRequested(ports)(c.sfPos).flatMap {
      case ScienceFold.Parked |
           ScienceFold.Position(_, _, BottomPort) => HrwfsPickupPosition.Parked.some
      case ScienceFold.Position(_, _, _) => none
    }
  }

  implicit private val encodeMountGuideConfig: EncodeEpicsValue[MountGuideOption, String] =
    EncodeEpicsValue {
      case MountGuideOption.MountGuideOn => "on"
      case MountGuideOption.MountGuideOff => "off"
    }

  def setMountGuide[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem], c: MountGuideOption, d: MountGuideOption
  ): Option[C => IO[C]] = applyParam(subsystems.contains(Subsystem.Mount), c, d,
    (x: MountGuideOption) => TcsEpics.instance.mountGuideCmd.setMode(encode(x)),
    l ^|-> BaseEpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.mountGuide
  )

  implicit private val encodeM1GuideConfig: EncodeEpicsValue[M1GuideConfig, String] =
    EncodeEpicsValue {
      case M1GuideConfig.M1GuideOn(_) => "on"
      case M1GuideConfig.M1GuideOff => "off"
    }

  def setM1Guide[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem], c: M1GuideConfig, d: M1GuideConfig
  ): Option[C => IO[C]] = applyParam(subsystems.contains(Subsystem.M1), c, d,
    (x: M1GuideConfig) => TcsEpics.instance.m1GuideCmd.setState(encode(x)),
    l ^|-> BaseEpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m1Guide
  )

  private val encodeM2Guide: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(_, _) => "on"
      case M2GuideConfig.M2GuideOff => "off"
    }

  private val encodeM2Coma: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(ComaOption.ComaOn, _) => "on"
      case _ => "off"
    }

  private val encodeM2GuideReset: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(_, _) => "off"
      case M2GuideConfig.M2GuideOff => "on"
    }

  def setM2Guide[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem], c: M2GuideConfig, d: M2GuideConfig
  ): Option[C => IO[C]] = {
    val actions = List(
      TcsEpics.instance.m2GuideModeCmd.setComa(encodeM2Coma.encode(d))
        .whenA(encodeM2Coma.encode(d) =!= encodeM2Coma.encode(c)),
      TcsEpics.instance.m2GuideCmd.setState(encodeM2Guide.encode(d))
        .whenA(encodeM2Guide.encode(d) =!= encodeM2Guide.encode(c)),
      TcsEpics.instance.m2GuideConfigCmd.setReset(encodeM2GuideReset.encode(d))
        .whenA(encodeM2GuideReset.encode(d) =!= encodeM2GuideReset.encode(c))
    )
    
    (subsystems.contains(Subsystem.M2) && actions.nonEmpty).option( x =>
      actions.sequence *> IO(
        (l ^|-> BaseEpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m2Guide).set(d)(x)
      )
    )
  }

  def setScienceFold[C](l: Lens[C, BaseEpicsTcsConfig])(subsystems: NonEmptySet[Subsystem], c: C, d: LightPath)
  : Option[C => IO[C]] = {
    val base = l.get(c)
    scienceFoldFromRequested(base.instPorts)(d).flatMap { sf =>
      (subsystems.contains(Subsystem.AGUnit) && base.scienceFoldPosition.forall(_ =!= sf)).option { x =>
        setScienceFoldConfig(sf) *> IO((l ^|-> BaseEpicsTcsConfig.scienceFoldPosition).set(sf.some)(x))
      }
    }
  }

  /*
   * Positions Parked and OUT are equivalent for practical purposes. Therefore, if the current position is Parked and
   * requested position is OUT (or the other way around), then is not necessary to move the HR pickup mirror.
   */
  def setHrPickup[C](l: Lens[C, BaseEpicsTcsConfig])(
    subsystems: NonEmptySet[Subsystem], current: C, d: AGConfig
  ): Option[C => IO[C]] = {
    val base = l.get(current)
    subsystems.contains(Subsystem.AGUnit).fold(
      calcHrPickupPosition(d, base.instPorts).flatMap { a =>
        val b = base.hrwfsPickupPosition
        (a =!= b && (HrwfsPickupPosition.isInTheWay(a) || HrwfsPickupPosition.isInTheWay(b))).option { x =>
          setHRPickupConfig(a) *> IO((l ^|-> BaseEpicsTcsConfig.hrwfsPickupPosition).set(a)(x))
        }
      },
      none
    )
  }

  val tcsTimeout: Time = Seconds(60)
  val agTimeout: Time = Seconds(60)

  private def configBaseParams(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, tcs: BasicTcsConfig)
  : List[BaseEpicsTcsConfig => IO[BaseEpicsTcsConfig]] = List(
    setPwfs1Probe(Lens.id)(subsystems, current.pwfs1.tracking, tcs.gds.pwfs1.tracking),
    setPwfs2Probe(Lens.id)(subsystems, current.pwfs2.tracking, tcs.gds.pwfs2.tracking),
    setOiwfsProbe(Lens.id)(subsystems, current.oiwfs.tracking, tcs.gds.oiwfs.tracking),
    tcs.tc.offsetA.flatMap(o => applyParam(subsystems.contains(Subsystem.Mount), current.offset,
      o.toFocalPlaneOffset(current.iaa), setTelescopeOffset, BaseEpicsTcsConfig.offset
    )),
    tcs.tc.wavelA.flatMap(applyParam(subsystems.contains(Subsystem.Mount), current.wavelA, _, setWavelength,
      BaseEpicsTcsConfig.wavelA
    )),
    setScienceFold(Lens.id)(subsystems, current, tcs.agc.sfPos),
    setHrPickup(Lens.id)(subsystems, current, tcs.agc)
  ).mapFilter(identity)

  def applyParam[F[_] : Applicative, T: Eq, C](used: Boolean,
                                            current: T,
                                            demand: T,
                                            act: T => F[Unit],
                                            lens: Lens[C, T])
  : Option[C => F[C]] =
    (used && current =!= demand).option(c => act(demand) *> lens.set(demand)(c).pure[F])

  private def guideParams(subsystems: NonEmptySet[Subsystem], current: BaseEpicsTcsConfig, demand: BasicTcsConfig)
  : List[BaseEpicsTcsConfig => IO[BaseEpicsTcsConfig]] = List(
    setMountGuide(Lens.id)(subsystems, current.telescopeGuideConfig.mountGuide, demand.gc.mountGuide),
    setM1Guide(Lens.id)(subsystems, current.telescopeGuideConfig.m1Guide, demand.gc.m1Guide),
    setM2Guide(Lens.id)(subsystems, current.telescopeGuideConfig.m2Guide, demand.gc.m2Guide),
    setPwfs1(Lens.id)(subsystems, current.pwfs1.detector, demand.gds.pwfs1.detector),
    setPwfs2(Lens.id)(subsystems, current.pwfs2.detector, demand.gds.pwfs2.detector),
    setOiwfs(Lens.id)(subsystems, current.oiwfs.detector, demand.gds.oiwfs.detector)
  ).mapFilter(identity)

  // Disable M1 guiding if source is off
  private def normalizeM1Guiding(gaosEnabled: Boolean): Endo[BasicTcsConfig] = cfg =>
    (BasicTcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
      case g @ M1GuideConfig.M1GuideOn(src) => src match {
        case M1Source.PWFS1 => if(guiderActive(cfg.gds.pwfs1)) g else M1GuideConfig.M1GuideOff
        case M1Source.PWFS2 => if(guiderActive(cfg.gds.pwfs2)) g else M1GuideConfig.M1GuideOff
        case M1Source.OIWFS => if(guiderActive(cfg.gds.oiwfs)) g else M1GuideConfig.M1GuideOff
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
          case TipTiltSource.PWFS1 => guiderActive(cfg.gds.pwfs1)
          case TipTiltSource.PWFS2 => guiderActive(cfg.gds.pwfs2)
          case TipTiltSource.OIWFS => guiderActive(cfg.gds.oiwfs)
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
          .set(calc(current.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
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

}
