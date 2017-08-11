// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.server.ConfigUtilOps._
import edu.gemini.seqexec.server.GcalController._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.calunit.CalUnitConstants._
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Lamp
import edu.gemini.spModel.seqcomp.SeqConfigNames.{CALIBRATION_CONFIG_NAME, CALIBRATION_KEY}

import scala.collection.JavaConverters._
import scala.Function.const
import scalaz.Scalaz._
import scalaz.\/

/**
  * Created by jluhrs on 3/21/17.
  */
final case class Gcal(controller: GcalController, isCP: Boolean) extends System {
  import Gcal._

  override val name: String = CALIBRATION_CONFIG_NAME

  /**
    * Called to configure a system, returns a Task[SeqexecFailure \/ ConfigResult]
    */
  override def configure(config: Config): SeqAction[ConfigResult] = ^(controller.getConfig, fromSequenceConfig(config, isCP))(diffConfiguration)
    .flatMap(controller.applyConfig).map(const(ConfigResult(this)))
}

object Gcal {
  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  implicit class Recover[T](v: \/[ConfigUtilOps.ExtractFailure, T]) {
    def recoverWithDefault[R >:T](d: R): TrySeq[R] = v.recoverWith[ConfigUtilOps.ExtractFailure, R] {
          case ConfigUtilOps.KeyNotFound(_)            => d.right[ConfigUtilOps.ExtractFailure]
          case e @ ConfigUtilOps.ConversionError(_, _) => e.left
        }.leftMap(explainExtractError)
  }

  def diffConfiguration(from: GcalConfig, to: GcalConfig): GcalConfig = {

    def diff[T](from: Option[T], to: Option[T]): Option[T] = (from, to) match {
      case (Some(a), Some(b)) if a != b => Some(b)
      case (None, Some(b))              => Some(b)
      case _                            => None
    }

    GcalConfig(
      diff(from.lampAr, to.lampAr),
      diff(from.lampCuAr, to.lampCuAr),
      diff(from.lampQh, to.lampQh),
      diff(from.lampThAr, to.lampThAr),
      diff(from.lampXe, to.lampXe),
      diff(from.lampIr, to.lampIr),
      diff(from.shutter, to.shutter),
      diff(from.filter, to.filter),
      diff(from.diffuser, to.diffuser)
    )
  }

  def fromSequenceConfig(config: Config, isCP: Boolean): SeqAction[GcalConfig] = {
    val lamps = config.extract(CALIBRATION_KEY / LAMP_PROP).as[java.util.Set[Lamp]].map(_.asScala.toList).recoverWithDefault(List.empty)

    val arLamp = lamps.map(v => if (v.contains(Lamp.AR_ARC)) Some(LampState.On) else Some(LampState.Off))
    val cuarLamp = lamps.map(v => if (v.contains(Lamp.CUAR_ARC)) Some(LampState.On) else Some(LampState.Off))
    val tharLamp = lamps.map(v => if (v.contains(Lamp.THAR_ARC)) Some(LampState.On) else Some(LampState.Off))
    val qhLamp = lamps.map(v => if (v.contains(Lamp.QUARTZ)) Some(LampState.On) else Some(LampState.Off))
    val xeLamp = lamps.map(v => if (v.contains(Lamp.XE_ARC)) Some(LampState.On) else Some(LampState.Off))
    val irLampCP = lamps.map(v => if (v.contains(Lamp.IR_GREY_BODY_HIGH) || v.contains(Lamp.IR_GREY_BODY_LOW)) Some(LampState.On) else None)
    val irLampMK = lamps.map(v => if (v.contains(Lamp.IR_GREY_BODY_HIGH)) Some(LampState.On)
                                  else if (v.contains(Lamp.IR_GREY_BODY_LOW)) Some(LampState.Off) else None)
    val shutter = config.extract(CALIBRATION_KEY / SHUTTER_PROP).as[Shutter].map(Some(_)).recoverWithDefault(None)
    val filter = config.extract(CALIBRATION_KEY / FILTER_PROP).as[Filter].map(Some(_)).recoverWithDefault(None)
    val diffuser = config.extract(CALIBRATION_KEY / DIFFUSER_PROP).as[Diffuser].map(Some(_)).recoverWithDefault(None)

    SeqAction.either(
      for {
        lm   <- lamps
        ar   <- arLamp.map(_.map(ArLampState))
        cuar <- cuarLamp.map(_.map(CuArLampState))
        thar <- tharLamp.map(_.map(ThArLampState))
        qh   <- qhLamp.map(_.map(QHLampState))
        xe   <- xeLamp.map(_.map(XeLampState))
        ir   <- (if (isCP) irLampCP else irLampMK).map(_.map(IrLampState))
        sht  <- shutter
        flt  <- filter
        dif  <- diffuser
      } yield if(lamps.isEmpty && List(sht, flt, dif).all(_.isEmpty)) GcalConfig.allOff
              else GcalConfig(ar, cuar, qh, thar, xe, ir, sht, flt, dif)
    )

  }
}
