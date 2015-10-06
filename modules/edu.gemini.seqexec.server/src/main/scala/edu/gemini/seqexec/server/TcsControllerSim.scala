package edu.gemini.seqexec.server

import java.util.logging.{Level, Logger}

import edu.gemini.seqexec.server.TaskRef._
import edu.gemini.seqexec.server.TcsController._
import edu.gemini.spModel.core.Wavelength
import squants.space.{Degrees, Nanometers, Millimeters}

import scalaz.concurrent.Task

/**
 * Created by jluhrs on 8/3/15.
 */
object TcsControllerSim extends TcsController {

  val guideState = newTaskRef(GuideConfig(MountGuideOff, M1GuideOff, M2GuideOff))
  val telescopeState = newTaskRef(TelescopeConfig(
    OffsetA(FocalPlaneOffset(OffsetX(Millimeters(0.0)), OffsetY(Millimeters(0.0)))),
    OffsetB(FocalPlaneOffset(OffsetX(Millimeters(0.0)), OffsetY(Millimeters(0.0)))),
    OffsetC(FocalPlaneOffset(OffsetX(Millimeters(0.0)), OffsetY(Millimeters(0.0)))),
    WavelengthA(Wavelength(Nanometers(445))),
    WavelengthB(Wavelength(Nanometers(445))),
    WavelengthC(Wavelength(Nanometers(445))),
    Beam.A))
  val guidersTrackState = newTaskRef(GuidersTrackingConfig(
    ProbeTrackingConfigP1(ProbeTrackingConfig.Parked),
    ProbeTrackingConfigP2(ProbeTrackingConfig.Parked),
    ProbeTrackingConfigOI(ProbeTrackingConfig.Parked),
    ProbeTrackingConfigAO(ProbeTrackingConfig.Parked)))
  val guidersActivityState = newTaskRef(GuidersEnabled(
    GuiderSensorOptionP1(GuiderSensorOff),
    GuiderSensorOptionP2(GuiderSensorOff),
    GuiderSensorOptionOI(GuiderSensorOff),
    GuiderSensorOptionAO(GuiderSensorOff)))
  val agState = newTaskRef(AGConfig(ScienceFoldPosition.Parked, HrwfsPickupPosition.Parked))
  val iaaState = newTaskRef(InstrumentAlignAngle(Degrees(0.0)))
  private val Log = Logger.getLogger(getClass.getName)

  override def getConfig: SeqAction[TcsConfig] = for {
    a <- guideState.flatMap(_.get)
    b <- telescopeState.flatMap(_.get)
    c <- guidersTrackState.flatMap(_.get)
    d <- guidersActivityState.flatMap(_.get)
    e <- agState.flatMap(_.get)
    f <- iaaState.flatMap(_.get)
  } yield TrySeq(TcsConfig(a, b, c, d, e, f))

  override def applyConfig(tc: TelescopeConfig, gtc: GuidersTrackingConfig, ge: GuidersEnabled, agc: AGConfig): SeqAction[Unit] =
    for {
      _ <- Task {
        Log.log(Level.INFO, "Applying TCS configuration")
        Thread.sleep(2000)
      }
      _ <- telescopeState.flatMap(_.put(tc))
      _ <- guidersTrackState.flatMap(_.put(gtc))
      _ <- guidersActivityState.flatMap(_.put(ge))
      _ <- agState.flatMap(_.put(agc))
    } yield TrySeq(())

  override def guide(gc: GuideConfig): SeqAction[Unit] =
    for {
      _ <- Task {
        Log.log(Level.INFO, "Applying guiding configuration")
        Thread.sleep(1000)
      }
      _ <- guideState.flatMap(_.put(gc))
    } yield TrySeq(())

}
