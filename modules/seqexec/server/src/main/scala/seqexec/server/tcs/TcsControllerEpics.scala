// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Endo
import seqexec.server.tcs.TcsController.{HrwfsConfig, _}
import seqexec.server.{EpicsCodex, EpicsCommand, SeqAction}
import edu.gemini.spModel.core.Wavelength
import org.log4s.getLogger
import squants.time.Seconds
import cats.data._
import cats.effect.IO
import cats.implicits._
import seqexec.server.altair.Altair
import seqexec.server.gems.Gems
import shapeless.tag
import squants.{Angle, Length}
import monocle.Iso
import shapeless.tag.@@

object TcsControllerEpics extends TcsController {
  private val Log = getLogger

  import EpicsCodex._
  import MountGuideOption._
  import ScienceFoldPositionCodex._

  private def setTelescopeConfig(c: TelescopeConfig, iaa: Angle): SeqAction[Unit] = {
    val off = c.offsetA.map{ v =>
      ((- v.p * iaa.cos - v.q * iaa.sin) / FOCAL_PLANE_SCALE, (v.p * iaa.sin - v.q * iaa.cos) / FOCAL_PLANE_SCALE)
    }

    off.map { case (x, y) => TcsEpics.instance.offsetACmd.setX(x.toMillimeters) *>
      TcsEpics.instance.offsetACmd.setY(y.toMillimeters)
    }.getOrElse(SeqAction.void) *>
      c.wavelA.map(w => TcsEpics.instance.wavelSourceA.setWavel(w.toMicrons)).getOrElse(SeqAction.void)
  }

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
    val sf = setScienceFoldConfig(c.sfPos)
    val hr = c.hrwfs.map {
      case HrwfsConfig.Manual(h) => setHRPickupConfig(h)
      case HrwfsConfig.Auto      => c.sfPos match {
        case ScienceFoldPosition.Position(_, sink) => EitherT.liftF(portFromSinkName(sink))
          .flatMap(_.map(p => if(p === BottomPort) setHRPickupConfig(HrwfsPickupPosition.Parked)
                              else SeqAction.void
          ).getOrElse(SeqAction.void))
        case _                                     => SeqAction.void
      }
    }.getOrElse(SeqAction.void)

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

  private def willMove(current: EpicsTcsConfig, demand: TcsConfig): Boolean =
    demand.tc.offsetA.exists(o => o.p.value =!= current.offsetP.value || o.q.value =!= current.offsetQ.value )

  override def applyConfig(subsystems: NonEmptyList[Subsystem],
                           gaos: Option[Either[Altair[IO], Gems[IO]]],
                           tcs: TcsConfig): SeqAction[Unit] = {
    def configSubsystem(subsystem: Subsystem, tcs: TcsConfig, iaa: Angle): SeqAction[Unit] = subsystem match {
      case Subsystem.OIWFS  =>
        setProbeTrackingConfig(TcsEpics.instance.oiwfsProbeGuideCmd, tcs.gds.oiwfs.tracking)
      case Subsystem.P1WFS  =>
        setProbeTrackingConfig(TcsEpics.instance.pwfs1ProbeGuideCmd, tcs.gds.pwfs1.tracking)
      case Subsystem.P2WFS  =>
        setProbeTrackingConfig(TcsEpics.instance.pwfs2ProbeGuideCmd, tcs.gds.pwfs2.tracking)
      case Subsystem.Mount  => setTelescopeConfig(tcs.tc, iaa)
      case Subsystem.AGUnit => setAGUnit(tcs.agc)
      case _                => SeqAction.void
    }

    def sysConfig(iaa: Angle): SeqAction[Unit] = subsystems.tail.foldLeft(configSubsystem(subsystems.head, tcs, iaa))(
      (b, a) => b *> configSubsystem(a, tcs, iaa)) *>
      TcsEpics.instance.post *>
      EitherT.right(IO.apply(Log.debug("TCS configuration command post"))) *>
      (if(subsystems.toList.contains(Subsystem.Mount))
        TcsEpics.instance.waitInPosition(tcsTimeout) *> EitherT.right(IO.apply(Log.info("TCS inposition")))
      else if(subsystems.toList.intersect(List(Subsystem.P1WFS, Subsystem.P2WFS, Subsystem.AGUnit)).nonEmpty)
        TcsEpics.instance.waitAGInPosition(agTimeout) *> EitherT.right(IO.apply(Log.debug("AG inposition")))
      else SeqAction.void)

    for {
      curr <- TcsConfigRetriever.retrieveConfiguration
      _    <- guideOff(curr, tcs)
      _    <- sysConfig(curr.iaa)
      _    <- guide(tcs)
    } yield ()
  }

  def guide(c: TcsConfig): SeqAction[Unit] = for {
    _ <- setMountGuide(c.gc.mountGuide)
    _ <- setM1Guide(c.gc.m1Guide)
    _ <- setM2Guide(c.gc.m2Guide)
    _ <- setGuiderWfs(TcsEpics.instance.pwfs1ObserveCmd, TcsEpics.instance.pwfs1StopObserveCmd, c.gds.pwfs1.detector)
    _ <- setGuiderWfs(TcsEpics.instance.pwfs2ObserveCmd, TcsEpics.instance.pwfs2StopObserveCmd, c.gds.pwfs2.detector)
    _ <- setGuiderWfs(TcsEpics.instance.oiwfsObserveCmd, TcsEpics.instance.oiwfsStopObserveCmd, c.gds.oiwfs.detector)
    _ <- TcsEpics.instance.post
    _ <- EitherT.right(IO.apply(Log.info("TCS guide command post")))
  } yield ()

  def calcGuideOff(current: EpicsTcsConfig, demand: TcsConfig): TcsConfig = {
    if(willMove(current, demand)) (TcsConfig.gds.modify(
      (GuidersConfig.pwfs1 ^<-> Iso.apply[GuiderConfig@@P1Config, GuiderConfig](x => x)(tag[P1Config](_)) ^|-> GuiderConfig.detector).set(GuiderSensorOff) >>>
        (GuidersConfig.pwfs2 ^<-> Iso.apply[GuiderConfig@@P2Config, GuiderConfig](x => x)(tag[P2Config](_)) ^|-> GuiderConfig.detector).set(GuiderSensorOff) >>>
        (GuidersConfig.oiwfs ^<-> Iso.apply[GuiderConfig@@OIConfig, GuiderConfig](x => x)(tag[OIConfig](_))^|-> GuiderConfig.detector).set(GuiderSensorOff)
    ) >>> normalizeM1Guiding >>> normalizeM2Guiding >>> normalizeMountGuiding)(demand)
    else demand
  }

  def guideOff(current: EpicsTcsConfig, demand: TcsConfig): SeqAction[Unit] = guide(calcGuideOff(current, demand))

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

  final case class EpicsTcsConfig(
    iaa: Angle,
    offsetX: Length,
    offsetY: Length,
    wavelA: Wavelength,
    pwfs1: GuiderConfig,
    pwfs2: GuiderConfig,
    oiwfs: GuiderConfig,
    telescopeGuideConfig: TelescopeGuideConfig,
    aoFold: AoFold,
    scienceFoldPosition: ScienceFoldPosition,
    hrwfsPickupPosition: HrwfsPickupPosition
  ) {
    val offsetP: Angle = (-offsetX * iaa.cos + offsetY * iaa.sin) * FOCAL_PLANE_SCALE
    val offsetQ: Angle = (-offsetX * iaa.sin - offsetY * iaa.cos) * FOCAL_PLANE_SCALE
  }

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
      case M2GuideOn(coma, srcs) => {
        val ss = srcs.filter{
          case TipTiltSource.PWFS1 => guiderActive(cfg.gds.pwfs1)
          case TipTiltSource.PWFS2 => guiderActive(cfg.gds.pwfs2)
          case TipTiltSource.OIWFS => guiderActive(cfg.gds.oiwfs)
          case _                   => true
        }
        if(ss.isEmpty) M2GuideOff
        else M2GuideOn(coma, ss)
      }
      case x                     => x
    }(cfg)

  // Disable Mount guiding if M2 guiding is disabled
  val normalizeMountGuiding: Endo[TcsConfig] = cfg =>
    (TcsConfig.gc ^|-> TelescopeGuideConfig.mountGuide).modify{ m => (m, cfg.gc.m2Guide) match {
      case (MountGuideOn, M2GuideOn(_, _)) => MountGuideOn
      case _                               => MountGuideOff
    } }(cfg)


}
