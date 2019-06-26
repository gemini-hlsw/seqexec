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
import cats.{Applicative, Eq}
import monocle.{Iso, Lens}
import squants.Length
import seqexec.model.enum.ComaOption
import seqexec.model.enum.MountGuideOption
import seqexec.model.M2GuideConfig
import seqexec.model.M1GuideConfig
import seqexec.model.TelescopeGuideConfig
import seqexec.server.tcs.TcsController._
import seqexec.server.{EpicsCodex, EpicsCommand}
import seqexec.server.tcs.TcsEpics.{ProbeFollowCmd, ProbeGuideCmd}
import shapeless.tag
import shapeless.tag.@@
import squants.space.Arcseconds

/*
 * Base implementation of an Epics TcsController
 * Type parameter C is the class used to hold the current configuration
 */
trait TcsControllerEpics[C] {

  import TcsControllerEpics._
  import EpicsCodex._
  import ScienceFoldPositionCodex._

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
      case NodChopTrackingOption.NodChopTrackingOn  => "On"
      case NodChopTrackingOption.NodChopTrackingOff => "Off"
    }

  implicit val encodeFollowOption: EncodeEpicsValue[FollowOption, String] =
    EncodeEpicsValue {
      case FollowOption.FollowOn  => "On"
      case FollowOption.FollowOff => "Off"
    }

  def setNodChopProbeTrackingConfig[F[_] : Async](s: TcsEpics.ProbeGuideCmd[F])(
    c: NodChopTrackingConfig
  ): F[Unit] = for {
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

  private def setGuideProbe[F[_] : Async](guideControl: GuideControl[F], trkSet: ProbeTrackingConfig => C => C)
                                            (subsystems: NonEmptySet[Subsystem], c: ProbeTrackingConfig, d: ProbeTrackingConfig)
  : Option[C => F[C]] =
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

  def setPwfs1Probe(l: ProbeTrackingConfig => C => C)(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[C => IO[C]] =
    setGuideProbe(pwfs1GuiderControl, l)(a, b, c)

  private def pwfs2GuiderControl: GuideControl[IO] = GuideControl(Subsystem.PWFS2, TcsEpics.instance.pwfs2Park,
    TcsEpics.instance.pwfs2ProbeGuideCmd, TcsEpics.instance.pwfs2ProbeFollowCmd)

  def setPwfs2Probe(l: ProbeTrackingConfig => C => C)(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[C => IO[C]] =
    setGuideProbe(pwfs2GuiderControl, l)(a, b, c)

  private def oiwfsGuiderControl: GuideControl[IO] = GuideControl(Subsystem.OIWFS, TcsEpics.instance.oiwfsPark,
    TcsEpics.instance.oiwfsProbeGuideCmd, TcsEpics.instance.oiwfsProbeFollowCmd)

  def setOiwfsProbe(l: ProbeTrackingConfig => C => C)(
    a: NonEmptySet[Subsystem], b: ProbeTrackingConfig, c: ProbeTrackingConfig
  ): Option[C => IO[C]] =
    setGuideProbe(oiwfsGuiderControl, l)(a, b, c)

  private def setGuiderWfs[F[_] : Sync](on: TcsEpics.WfsObserveCmd[F], off: EpicsCommand)(c: GuiderSensorOption)
  : F[Unit] = {
    val NonStopExposures = -1
    c match {
      case GuiderSensorOff => off.mark
      case GuiderSensorOn => on.setNoexp(NonStopExposures) // Set number of exposures to non-stop (-1)
    }
  }

  def setPwfs1(o: GuiderSensorOption): IO[Unit] =
    setGuiderWfs(TcsEpics.instance.pwfs1ObserveCmd, TcsEpics.instance.pwfs1StopObserveCmd)(o)

  def setPwfs2(o: GuiderSensorOption): IO[Unit] =
    setGuiderWfs(TcsEpics.instance.pwfs2ObserveCmd, TcsEpics.instance.pwfs2StopObserveCmd)(o)

  def setOiwfs(o: GuiderSensorOption): IO[Unit] =
    setGuiderWfs(TcsEpics.instance.oiwfsObserveCmd, TcsEpics.instance.oiwfsStopObserveCmd)(o)

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
    portFromSinkName(ports)(r.sink).map { p =>
      if (p === BottomPort && r.source === LightSource.Sky) ScienceFold.Parked
      else ScienceFold.Position(r.source, r.sink, p)
    }

  def setScienceFoldConfig(sfPos: ScienceFold): IO[Unit] = sfPos match {
    case ScienceFold.Parked => TcsEpics.instance.scienceFoldParkCmd.mark[IO]
    case p: ScienceFold.Position => TcsEpics.instance.scienceFoldPosCmd.setScfold(encode(p))
  }

  implicit private val encodeHrwfsPickupPosition: EncodeEpicsValue[HrwfsPickupPosition, String] =
    EncodeEpicsValue{
      case HrwfsPickupPosition.IN     => "IN"
      case HrwfsPickupPosition.OUT    => "OUT"
      case HrwfsPickupPosition.Parked => "park-pos."
    }

  def setHRPickupConfig(hrwfsPos: HrwfsPickupPosition): IO[Unit] = hrwfsPos match {
    case HrwfsPickupPosition.Parked => TcsEpics.instance.hrwfsParkCmd.mark[IO]
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
      case MountGuideOption.MountGuideOn  => "on"
      case MountGuideOption.MountGuideOff => "off"
    }

  def setMountGuide(c: MountGuideOption): IO[Unit] =
    TcsEpics.instance.mountGuideCmd.setMode(encode(c))

  implicit private val encodeM1GuideConfig: EncodeEpicsValue[M1GuideConfig, String] =
    EncodeEpicsValue {
      case M1GuideConfig.M1GuideOn(_) => "on"
      case M1GuideConfig.M1GuideOff => "off"
    }

  def setM1Guide(c: M1GuideConfig): IO[Unit] = TcsEpics.instance.m1GuideCmd.setState(encode(c))

  private val encodeM2Guide: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(_, _) => "on"
      case M2GuideConfig.M2GuideOff      => "off"
    }

  private val encodeM2Coma: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(ComaOption.ComaOn, _) => "on"
      case _                                             => "off"
    }

  private val encodeM2GuideReset: EncodeEpicsValue[M2GuideConfig, String] =
    EncodeEpicsValue {
      case M2GuideConfig.M2GuideOn(_, _) => "off"
      case M2GuideConfig.M2GuideOff => "on"
    }

  def setM2Guide(c: M2GuideConfig, d: M2GuideConfig, l: Lens[C, TelescopeGuideConfig])(tcsCfg: C): IO[C] = {
    val actions = List(
      TcsEpics.instance.m2GuideModeCmd.setComa(encodeM2Coma.encode(d))
        .whenA(encodeM2Coma.encode(d) =!= encodeM2Coma.encode(c)),
      TcsEpics.instance.m2GuideCmd.setState(encodeM2Guide.encode(d))
        .whenA(encodeM2Guide.encode(d) =!= encodeM2Guide.encode(c)),
      TcsEpics.instance.m2GuideConfigCmd.setReset(encodeM2GuideReset.encode(d))
        .whenA(encodeM2GuideReset.encode(d) =!= encodeM2GuideReset.encode(c))
    )

    actions.sequence.unlessA(actions.isEmpty) *> IO(
      (l ^|-> TelescopeGuideConfig.m2Guide).set(d)(tcsCfg)
    )
  }

  def setScienceFold(subsystems: NonEmptySet[Subsystem], c: C, d: LightPath,
                             l: Lens[C, Option[ScienceFold]], ports: InstrumentPorts)
  : Option[C => IO[C]] = scienceFoldFromRequested(ports)(d).flatMap { sf =>
    (subsystems.contains(Subsystem.AGUnit) && l.get(c).forall(_ =!= sf)).option { x =>
      setScienceFoldConfig(sf) *> IO(l.set(sf.some)(x))
    }
  }

  /*
   * Positions Parked and OUT are equivalent for practical purposes. Therefore, if the current position is Parked and
   * requested position is OUT (or the other way around), then is not necessary to move the HR pickup mirror.
   */
  def setHrPickup(subsystems: NonEmptySet[Subsystem], c: AGConfig, current: C, l: Lens[C, HrwfsPickupPosition],
                          ports: InstrumentPorts)
  : Option[C => IO[C]] =
    subsystems.contains(Subsystem.AGUnit).fold(
      calcHrPickupPosition(c, ports).flatMap { a =>
        val b = l.get(current)
        (a =!= b && (HrwfsPickupPosition.isInTheWay(a) || HrwfsPickupPosition.isInTheWay(b))).option { x =>
          setHRPickupConfig(a) *> IO(l.set(a)(x))
        }
      },
      none
    )

  val tcsTimeout = Seconds(60)
  val agTimeout = Seconds(60)

  def applyParam[F[_] : Applicative, T: Eq](used: Boolean,
                                            current: T,
                                            demand: T,
                                            act: T => F[Unit],
                                            lens: Lens[C, T])
  : Option[C => F[C]] =
    (used && current =!= demand).option(c => act(demand) *> lens.set(demand)(c).pure[F])
}

object TcsControllerEpics {

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
                                             parkCmd: EpicsCommand,
                                             nodChopGuideCmd: ProbeGuideCmd[F],
                                             followCmd: ProbeFollowCmd[F]
                                            )

  val pwfs1OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE
  val pwfs2OffsetThreshold: Length = Arcseconds(0.01)/FOCAL_PLANE_SCALE

}
