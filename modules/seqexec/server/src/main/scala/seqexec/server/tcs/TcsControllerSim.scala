// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.{EitherT, NonEmptyList}
import cats.effect.IO
import seqexec.server.tcs.TcsController._
import seqexec.server.{SeqAction, TrySeq}
import edu.gemini.spModel.core.Wavelength
import org.log4s.getLogger
import squants.space.{Degrees, Millimeters, Nanometers}

import cats.implicits._

object TcsControllerSim extends TcsController {

  import MountGuideOption._

  private val Log = getLogger

  override def getConfig: SeqAction[TcsConfig] = SeqAction(TcsConfig(
    GuideConfig(MountGuideOff, M1GuideOff, M2GuideOff),
    TelescopeConfig(
      OffsetA(FocalPlaneOffset(OffsetX(Millimeters(0.0)), OffsetY(Millimeters(0.0)))),
      OffsetB(FocalPlaneOffset(OffsetX(Millimeters(0.0)), OffsetY(Millimeters(0.0)))),
      OffsetC(FocalPlaneOffset(OffsetX(Millimeters(0.0)), OffsetY(Millimeters(0.0)))),
      WavelengthA(Wavelength(Nanometers(445))),
      WavelengthB(Wavelength(Nanometers(445))),
      WavelengthC(Wavelength(Nanometers(445))),
      Beam.A),
    GuidersTrackingConfig(
      ProbeTrackingConfigP1(ProbeTrackingConfig.Parked),
      ProbeTrackingConfigP2(ProbeTrackingConfig.Parked),
      ProbeTrackingConfigOI(ProbeTrackingConfig.Parked),
      ProbeTrackingConfigAO(ProbeTrackingConfig.Parked)),
    GuidersEnabled(
      GuiderSensorOptionP1(GuiderSensorOff),
      GuiderSensorOptionP2(GuiderSensorOff),
      GuiderSensorOptionOI(GuiderSensorOff)),
    AGConfig(ScienceFoldPosition.Parked.some, HrwfsPickupPosition.Parked.some),
    InstrumentAlignAngle(Degrees(0.0))
  ))

  override def applyConfig(subsystems: NonEmptyList[Subsystem], tc: TcsConfig): SeqAction[Unit] = {
    def configSubsystem(subsystem: Subsystem): IO[Unit] = IO.apply(Log.info(s"Applying $subsystem configuration."))

    EitherT(
      subsystems.tail.foldLeft(configSubsystem(subsystems.head))((b, a) => b *> configSubsystem(a)).map(TrySeq(_)))
  }

  override def guide(gc: GuideConfig): SeqAction[Unit] =
    EitherT ( for {
        _ <- IO {
          Log.info("Applying guiding configuration")
          Thread.sleep(1000)
        }
      } yield TrySeq(())
    )

  override def notifyObserveStart: SeqAction[Unit] = EitherT.right(IO(Log.info("Simulate TCS observe")))

  override def notifyObserveEnd: SeqAction[Unit] = EitherT.right(IO(Log.info("Simulate TCS endObserve")))
}
