// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import java.util.{ Set => JSet }

import scala.Function.const
import scala.jdk.CollectionConverters._

import cats._
import cats.effect.Sync
import cats.syntax.all._
import edu.gemini.spModel.gemini.calunit.CalUnitConstants._
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Lamp
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Shutter
import seqexec.model.enum.Resource
import seqexec.server.CleanConfig
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigResult
import seqexec.server.ConfigUtilOps
import seqexec.server.ConfigUtilOps._
import seqexec.server.SeqexecFailure
import seqexec.server.System
import seqexec.server.gcal.GcalController._

/**
 * Created by jluhrs on 3/21/17.
 */
final case class Gcal[F[_]: Sync] private (controller: GcalController[F], cfg: GcalConfig)
    extends System[F] {

  override val resource: Resource = Resource.Gcal

  /**
   * Called to configure a system, returns a F[ConfigResult]
   */
  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    controller.applyConfig(cfg).map(const(ConfigResult(this)))

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def notifyObserveEnd: F[Unit] = Sync[F].unit

}

object Gcal {
  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  implicit val shutterEq: Eq[Shutter] = Eq.by(_.ordinal)

  def fromConfig[F[_]: Sync](
    isCP:   Boolean,
    config: CleanConfig
  ): F[GcalController[F] => Gcal[F]] = {
    val lamps: Either[ConfigUtilOps.ExtractFailure, List[Lamp]] = config
      .extractCalibrationAs[JSet[Lamp]](LAMP_PROP)
      .map(_.asScala.toList)
      .recover { case ConfigUtilOps.KeyNotFound(_) => List.empty[Lamp] }

    val arLamp     = lamps.map(v => if (v.contains(Lamp.AR_ARC)) LampState.On else LampState.Off)
    val cuarLamp   = lamps.map(v => if (v.contains(Lamp.CUAR_ARC)) LampState.On else LampState.Off)
    val tharLamp   = lamps.map(v => if (v.contains(Lamp.THAR_ARC)) LampState.On else LampState.Off)
    val qh5WLamp   = lamps.map(v => if (v.contains(Lamp.QUARTZ_5W)) LampState.On else LampState.Off)
    val qh100WLamp =
      lamps.map(v => if (v.contains(Lamp.QUARTZ_100W)) LampState.On else LampState.Off)
    val xeLamp     = lamps.map(v => if (v.contains(Lamp.XE_ARC)) LampState.On else LampState.Off)
    val irLampCP   = lamps.map(v =>
      if (v.contains(Lamp.IR_GREY_BODY_HIGH) || v.contains(Lamp.IR_GREY_BODY_LOW))
        Some(LampState.On)
      else None
    )
    val irLampMK   = lamps.map(v =>
      if (v.contains(Lamp.IR_GREY_BODY_HIGH)) Some(LampState.On)
      else if (v.contains(Lamp.IR_GREY_BODY_LOW)) Some(LampState.Off)
      else None
    )
    val shutter    = config.extractCalibrationAs[Shutter](SHUTTER_PROP)
    val filter     = config.extractCalibrationAs[Filter](FILTER_PROP)
    val diffuser   = config.extractCalibrationAs[Diffuser](DIFFUSER_PROP)

    for {
      l     <- lamps
      ar    <- arLamp.map(ArLampState.apply)
      cuar  <- cuarLamp.map(CuArLampState.apply)
      thar  <- tharLamp.map(ThArLampState.apply)
      qh5   <- qh5WLamp.map(QH5WLampState.apply)
      qh100 <- qh100WLamp.map(QH100WLampState.apply)
      xe    <- xeLamp.map(XeLampState.apply)
      ir    <- (if (isCP) irLampCP else irLampMK).map(_.map(IrLampState.apply))
      sht   <- shutter
      flt   <- filter
      dif   <- diffuser
    } yield { controller: GcalController[F] =>
      new Gcal[F](controller,
                  if (l.isEmpty && sht === Shutter.CLOSED) GcalConfig.GcalOff
                  else GcalConfig.GcalOn(ar, cuar, qh5, qh100, thar, xe, ir, sht, flt, dif)
      )
    }
  }.toF[F]

  // GCAL that always turn off its lamps except for the IR lamp. Used to assure GCAL light does not interfere in a non
  // calibration step
  def defaultGcal[F[_]: Sync](controller: GcalController[F]): Gcal[F] =
    new Gcal[F](controller, GcalConfig.GcalOffIgnoringIr)
}
