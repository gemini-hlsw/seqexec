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
import mouse.boolean._
import seqexec.server.altair.Altair
import seqexec.server.gems.Gems
import squants.Angle
import monocle.{Iso, Lens}
import monocle.macros.Lenses
import seqexec.server.tcs.Gaos.{GaosStarOff, GaosStarOn, OffsetMove, OffsetReached, OiOff, OiOn, P1Off, P1On, PauseCondition, ResumeCondition}
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
      case NodChopTrackingOption.NodChopTrackingOn  => "on"
      case NodChopTrackingOption.NodChopTrackingOff => "off"
    }

  private def setProbeTrackingConfig(s: TcsEpics.ProbeGuideCmd)(c: ProbeTrackingConfig): SeqAction[Unit] = for {
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

  // Special case: if source is the sky and the instrument is at the bottom port (port 1), the science fold must be parked.
  def setScienceFoldConfig(ports: InstrumentPorts)(sfPos: ScienceFoldPosition): SeqAction[Unit] = sfPos match {
    case ScienceFoldPosition.Parked => TcsEpics.instance.scienceFoldParkCmd.mark
    case p@ScienceFoldPosition.Position(LightSource.Sky, sink) =>
      portFromSinkName(ports)(sink).map(port =>
       if (port === BottomPort) TcsEpics.instance.scienceFoldParkCmd.mark
       else TcsEpics.instance.scienceFoldPosCmd.setScfold(encodeScienceFoldPosition(port).encode(p))
      ).getOrElse(SeqAction.void)
    case p: ScienceFoldPosition.Position =>
      portFromSinkName(ports)(p.sink).map(port =>
        TcsEpics.instance.scienceFoldPosCmd.setScfold(encodeScienceFoldPosition(port).encode(p))
      ).getOrElse(SeqAction.void)
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
    case HrwfsConfig.Auto      => c.sfPos match {
      case ScienceFoldPosition.Parked            => HrwfsPickupPosition.Parked.some
      case ScienceFoldPosition.Position(_, sink) => portFromSinkName(ports)(sink).flatMap(port =>
                                                      if(port === BottomPort) HrwfsPickupPosition.Parked.some
                                                      else None
                                                    )
      case _                                     => None
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

  implicit private val encodeM2GuideConfig: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue{
      case M2GuideOn(_, _) => "on"
      case M2GuideOff      => "off"
    }

  private def setM2Guide(c: M2GuideConfig): SeqAction[Unit] = TcsEpics.instance.m2GuideCmd.setState(encode(c))

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
      applyParam(subsystems.contains(Subsystem.PWFS1), current.pwfs1.tracking, tcs.gds.pwfs1.tracking,
        setProbeTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideCmd), EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.tracking),
      applyParam(subsystems.contains(Subsystem.PWFS2), current.pwfs2.tracking, tcs.gds.pwfs2.tracking,
        setProbeTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideCmd), EpicsTcsConfig.pwfs2 ^|-> GuiderConfig.tracking),
      applyParam(subsystems.contains(Subsystem.OIWFS), current.oiwfs.tracking, tcs.gds.oiwfs.tracking,
        setProbeTrackingConfig(TcsEpics.instance.oiwfsProbeGuideCmd), EpicsTcsConfig.oiwfs ^|-> GuiderConfig.tracking),
      tcs.tc.offsetA.flatMap(o => applyParam(subsystems.contains(Subsystem.Mount), current.offset,
        o.toFocalPlaneOffset(current.iaa), setTelescopeOffset, EpicsTcsConfig.offset)),
      tcs.tc.wavelA.flatMap(applyParam(subsystems.contains(Subsystem.Mount), current.wavelA, _, setWavelength,
        EpicsTcsConfig.wavelA)),
      applyParam(subsystems.contains(Subsystem.AGUnit), current.scienceFoldPosition, tcs.agc.sfPos,
        setScienceFoldConfig(current.instPorts), EpicsTcsConfig.scienceFoldPosition),
      calcHrPickupPosition(tcs.agc, current.instPorts).flatMap(applyParam(subsystems.contains(Subsystem.AGUnit),
        current.hrwfsPickupPosition, _, setHRPickupConfig, EpicsTcsConfig.hrwfsPickupPosition))
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
      s0 <- TcsConfigRetriever.retrieveConfiguration
      r  <- pauseGaos(gaos, s0, tcs)
      s1 <- guideOff(subsystems, s0, tcs)
      s2 <- sysConfig(s1)
      s3 <- guideOn(subsystems, s2, tcs)
      _  <- r(s3, tcs) //resume Gaos
    } yield ()
  }

  def guideParams(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsConfig)
  : List[EpicsTcsConfig => SeqAction[EpicsTcsConfig]] = List(
      applyParam(subsystems.contains(Subsystem.Mount), current.telescopeGuideConfig.mountGuide, demand.gc.mountGuide,
        setMountGuide, EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.mountGuide),
      applyParam(subsystems.contains(Subsystem.M1), current.telescopeGuideConfig.m1Guide, demand.gc.m1Guide,
        setM1Guide, EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m1Guide),
      applyParam(subsystems.contains(Subsystem.M2), current.telescopeGuideConfig.m2Guide, demand.gc.m2Guide,
        setM2Guide, EpicsTcsConfig.telescopeGuideConfig ^|-> TelescopeGuideConfig.m2Guide),
      applyParam(subsystems.contains(Subsystem.PWFS1), current.pwfs1.detector, demand.gds.pwfs1.detector,
        setPwfs1, EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector),
      applyParam(subsystems.contains(Subsystem.PWFS2), current.pwfs2.detector, demand.gds.pwfs2.detector,
        setPwfs2, EpicsTcsConfig.pwfs2 ^|-> GuiderConfig.detector),
      applyParam(subsystems.contains(Subsystem.OIWFS), current.oiwfs.detector, demand.gds.oiwfs.detector,
        setOiwfs, EpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector)
    ).collect{ case Some(x) => x }


  def tagIso[B, T]: Iso[B@@T, B] = Iso.apply[B@@T, B](x => x)(tag[T](_))

  def calcGuideOff(current: EpicsTcsConfig, demand: TcsConfig): TcsConfig = {
    val mustOff = willMove(current, demand)
    // Only turn things off here. Things that must be turned on will be turned on in GuideOn.
    def calc(c: GuiderSensorOption, d: GuiderSensorOption) = (mustOff || d === GuiderSensorOff).fold(GuiderSensorOff, c)

    (TcsConfig.gds.modify(
      (GuidersConfig.pwfs1 ^<-> tagIso ^|-> GuiderConfig.detector)
        .set(calc(current.pwfs1.detector, demand.gds.pwfs1.detector)) >>>
        (GuidersConfig.pwfs2 ^<-> tagIso ^|-> GuiderConfig.detector)
          .set(calc(current.pwfs2.detector, demand.gds.pwfs2.detector)) >>>
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
    ) >>> normalizeM1Guiding >>> normalizeM2Guiding >>> normalizeMountGuiding)(demand)
  }

  def calcAoPauseConditions(current: EpicsTcsConfig, demand: TcsConfig): Set[PauseCondition] = Set(
    demand.tc.offsetA.flatMap(v => (v =!= current.instrumentOffset)
      .option(OffsetMove(current.offset, v.toFocalPlaneOffset(current.iaa)))),
    (current.oiwfs.detector === GuiderSensorOn && demand.gds.oiwfs.detector === GuiderSensorOff).option(OiOff),
    (current.pwfs1.detector === GuiderSensorOn && demand.gds.pwfs1.detector === GuiderSensorOff).option(P1Off),
    (!demand.gds.aowfs).option(GaosStarOff)
  ).collect{ case Some(x) => x }

  def calcAoResumeConditions(current: EpicsTcsConfig, demand: TcsConfig): Set[ResumeCondition] = Set(
    demand.tc.offsetA.map(v => OffsetReached(v.toFocalPlaneOffset(current.iaa))),
    (demand.gds.oiwfs.detector === GuiderSensorOn).option(OiOn),
    (demand.gds.pwfs1.detector === GuiderSensorOn).option(P1On),
    demand.gds.aowfs.option(GaosStarOn)
  ).collect{ case Some(x) => x }

  def pauseGaos(gaos: Option[Either[Altair[IO], Gems[IO]]], current: EpicsTcsConfig, demand: TcsConfig)
  : SeqAction[(EpicsTcsConfig, TcsController.TcsConfig) => SeqAction[Unit]] = (gaos, demand.gaos).mapN {
    case (Left(g), c)  => SeqActionF.embed[IO, (EpicsTcsConfig, TcsController.TcsConfig) => SeqAction[Unit]](
      g.pause(c, calcAoPauseConditions(current, demand)).map(resumeGaos))
    case (Right(_), _) => SeqAction.fail[(EpicsTcsConfig, TcsController.TcsConfig) => SeqAction[Unit]](
      SeqexecFailure.Unexpected("GeMS not supported"))
    case _             => SeqAction((_: EpicsTcsConfig, _: TcsController.TcsConfig) => SeqAction.void)
  }.getOrElse(SeqAction((_, _) => SeqAction.void))

  def resumeGaos(resume: Set[ResumeCondition] => IO[Unit])(current: EpicsTcsConfig, demand: TcsConfig)
  : SeqAction[Unit] = SeqActionF.embed(resume(calcAoResumeConditions(current, demand)))


  def updateEpicsGuideConfig(epicsCfg: EpicsTcsConfig, demand: TcsConfig): EpicsTcsConfig = (
    EpicsTcsConfig.telescopeGuideConfig.set(demand.gc) >>>
      (EpicsTcsConfig.pwfs1 ^|-> GuiderConfig.detector).set(demand.gds.pwfs1.detector) >>>
      (EpicsTcsConfig.pwfs2 ^|-> GuiderConfig.detector).set(demand.gds.pwfs2.detector) >>>
      (EpicsTcsConfig.oiwfs ^|-> GuiderConfig.detector).set(demand.gds.oiwfs.detector)
  )(epicsCfg)

  def guideOff(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsConfig)
  : SeqAction[EpicsTcsConfig] = {
    val params = guideParams(subsystems, current, calcGuideOff (current, demand) )

    if(params.nonEmpty)
      for {
        s <- params.foldLeft(SeqAction(current)){ case (c, p) => c.flatMap(p)}
        _ <- TcsEpics.instance.post
        _ <- SeqAction(Log.info("Turning guide off"))
      } yield s
    else
      SeqAction(Log.info("Skipping guide off")) *> SeqAction(current)
  }

  def guideOn(subsystems: NonEmptySet[Subsystem], current: EpicsTcsConfig, demand: TcsConfig)
  : SeqAction[EpicsTcsConfig] = {

    // If the demand turned off any WFS, normalize will turn off the corresponding processing
    val normalizedGuiding = (normalizeM1Guiding >>> normalizeM2Guiding >>> normalizeMountGuiding)(demand)

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
  @Lenses
  final case class EpicsTcsConfig(
    iaa: Angle,
    offset: FocalPlaneOffset,
    wavelA: Wavelength,
    pwfs1: GuiderConfig,
    pwfs2: GuiderConfig,
    oiwfs: GuiderConfig,
    telescopeGuideConfig: TelescopeGuideConfig,
    aoFold: AoFold,
    scienceFoldPosition: ScienceFoldPosition,
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
  val normalizeM1Guiding: Endo[TcsConfig] = cfg =>
    (TcsConfig.gc ^|-> TelescopeGuideConfig.m1Guide).modify{
      case g@M1GuideOn(src) => src match {
        case M1Source.PWFS1 => if(guiderActive(cfg.gds.pwfs1)) g else M1GuideOff
        case M1Source.PWFS2 => if(guiderActive(cfg.gds.pwfs2)) g else M1GuideOff
        case M1Source.OIWFS => if(guiderActive(cfg.gds.oiwfs)) g else M1GuideOff
        case _              => g
      }
      case x                => x
    }(cfg)

  // Disable M2 sources if they are off, disable M2 guiding if all are off
  val normalizeM2Guiding: Endo[TcsConfig] = cfg =>
    (TcsConfig.gc ^|-> TelescopeGuideConfig.m2Guide).modify{
      case M2GuideOn(coma, srcs) =>
        val ss = srcs.filter{
          case TipTiltSource.PWFS1 => guiderActive(cfg.gds.pwfs1)
          case TipTiltSource.PWFS2 => guiderActive(cfg.gds.pwfs2)
          case TipTiltSource.OIWFS => guiderActive(cfg.gds.oiwfs)
          case _                   => true
        }
        if(ss.isEmpty) M2GuideOff
        else M2GuideOn(coma, ss)
      case x                     => x
    }(cfg)

  // Disable Mount guiding if M2 guiding is disabled
  val normalizeMountGuiding: Endo[TcsConfig] = cfg =>
    (TcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
      case (MountGuideOn, M2GuideOn(_, _)) => MountGuideOn
      case _                               => MountGuideOff
    } }(cfg)


}
