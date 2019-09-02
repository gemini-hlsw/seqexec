// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats._
import cats.implicits._
import cats.effect.Sync
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.calunit.CalUnitConstants._
import edu.gemini.spModel.gemini.calunit.CalUnitParams.{Lamp, Shutter}
import edu.gemini.spModel.seqcomp.SeqConfigNames.CALIBRATION_KEY
import java.util.{Set => JSet}

import org.log4s.{Logger, getLogger}

import scala.Function.const
import scala.collection.JavaConverters._
import seqexec.model.enum.Resource
import seqexec.server.ConfigUtilOps._
import seqexec.server.gcal.GcalController._
import seqexec.server.{ConfigResult, ConfigUtilOps, SeqexecFailure, System, TrySeq}

/**
  * Created by jluhrs on 3/21/17.
  */
final case class Gcal[F[_]](controller: GcalController[F], cfg: GcalConfig)(implicit val F: Sync[F]) extends System[F] {

  private val Log: Logger = getLogger

  override val resource: Resource = Resource.Gcal

  /**
    * Called to configure a system, returns a F[ConfigResult]
    */
  override def configure(config: Config): F[ConfigResult[F]] =
    for{
      _       <- F.delay(Log.info("Start GCAL configuration"))
      _       <- F.delay(Log.debug(s"GCAL configuration: ${cfg.show}"))
      ret     <- controller.applyConfig(cfg).map(const(ConfigResult(this)))
      _       <- F.delay(Log.info("Completed GCAL configuration"))
    } yield ret

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def notifyObserveEnd: F[Unit] = Sync[F].unit

}

object Gcal {
  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  implicit val shutterEq: Eq[Shutter] = Eq.by(_.ordinal)

  def fromConfig[F[_]: Sync](controller: GcalController[F], isCP: Boolean)(config: Config): TrySeq[Gcal[F]] = {
      val lamps: Either[ConfigUtilOps.ExtractFailure, List[Lamp]] = config.extractAs[JSet[Lamp]](CALIBRATION_KEY / LAMP_PROP)
        .map(_.asScala.toList)
        .recover{ case ConfigUtilOps.KeyNotFound(_) => List.empty[Lamp] }

      val arLamp = lamps.map(v => if (v.contains(Lamp.AR_ARC)) LampState.On else LampState.Off)
      val cuarLamp = lamps.map(v => if (v.contains(Lamp.CUAR_ARC)) LampState.On else LampState.Off)
      val tharLamp = lamps.map(v => if (v.contains(Lamp.THAR_ARC)) LampState.On else LampState.Off)
      val qhLamp = lamps.map(v => if (v.contains(Lamp.QUARTZ)) LampState.On else LampState.Off)
      val xeLamp = lamps.map(v => if (v.contains(Lamp.XE_ARC)) LampState.On else LampState.Off)
      val irLampCP = lamps.map(v => if (v.contains(Lamp.IR_GREY_BODY_HIGH) || v.contains(Lamp.IR_GREY_BODY_LOW)) Some(LampState.On) else None)
      val irLampMK = lamps.map(v => if (v.contains(Lamp.IR_GREY_BODY_HIGH)) Some(LampState.On)
                                    else if (v.contains(Lamp.IR_GREY_BODY_LOW)) Some(LampState.Off) else None)
      val shutter = config.extractAs[Shutter](CALIBRATION_KEY / SHUTTER_PROP)
      val filter = config.extractAs[Filter](CALIBRATION_KEY / FILTER_PROP)
      val diffuser = config.extractAs[Diffuser](CALIBRATION_KEY / DIFFUSER_PROP)

      for {
        _    <- lamps
        ar   <- arLamp.map(ArLampState.apply)
        cuar <- cuarLamp.map(CuArLampState.apply)
        thar <- tharLamp.map(ThArLampState.apply)
        qh   <- qhLamp.map(QHLampState.apply)
        xe   <- xeLamp.map(XeLampState.apply)
        ir   <- (if (isCP) irLampCP else irLampMK).map(_.map(IrLampState.apply))
        sht  <- shutter
        flt  <- filter
        dif  <- diffuser
      } yield new Gcal[F](controller,
        if(lamps.isEmpty && sht === Shutter.CLOSED ) GcalConfig.GcalOff
        else GcalConfig.GcalOn(ar, cuar, qh, thar, xe, ir, sht, flt, dif)
      )
    }.asTrySeq
}
