// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gcal

import cats._
import cats.implicits._
import cats.effect.IO
import cats.data.EitherT
import java.util.{Set => JSet}

import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.gemini.calunit.CalUnitConstants._
import edu.gemini.spModel.gemini.calunit.CalUnitParams.Lamp
import edu.gemini.spModel.seqcomp.SeqConfigNames.CALIBRATION_KEY
import org.log4s.{Logger, getLogger}

import scala.Function.const
import scala.collection.JavaConverters._
import seqexec.model.enum.Resource
import seqexec.server.ConfigUtilOps._
import seqexec.server.gcal.GcalController._
import seqexec.server.{ConfigResult, ConfigUtilOps, SeqAction, SeqexecFailure, System, TrySeq}
import seqexec.server._

/**
  * Created by jluhrs on 3/21/17.
  */
final case class Gcal(controller: GcalController, isCP: Boolean) extends System[IO] {
  import Gcal._

  private val Log: Logger = getLogger

  override val resource: Resource = Resource.Gcal

  /**
    * Called to configure a system, returns a IO[Either[SeqexecFailure, ConfigResult]]
    */
  override def configure(config: Config): SeqAction[ConfigResult[IO]] = for{
    _ <- EitherT.right(IO(Log.info("Start GCAL configuration")))
    reqCfg <- fromSequenceConfig(config, isCP)
    _ <- EitherT.right(IO(Log.debug(s"GCAL configuration: ${reqCfg.show}")))
    currCfg <- controller.getConfig
    ret <- controller.applyConfig(diffConfiguration(currCfg, reqCfg)).map(const(ConfigResult(this)))
    _ <- EitherT.right(IO(Log.info("Completed GCAL configuration")))
  } yield ret

  override def notifyObserveStart: SeqAction[Unit] = SeqAction.void

  override def notifyObserveEnd: SeqAction[Unit] = SeqAction.void
}

object Gcal {
  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  implicit class Recover[T](v: Either[ConfigUtilOps.ExtractFailure, T]) {
    def recoverWithDefault[R >:T](d: R): TrySeq[R] = v.recoverWith[ConfigUtilOps.ExtractFailure, R] {
          case ConfigUtilOps.KeyNotFound(_)            => d.asRight[ConfigUtilOps.ExtractFailure]
          case e @ ConfigUtilOps.ConversionError(_, _) => e.asLeft
        }.leftMap(explainExtractError)
  }

  def diffConfiguration(from: GcalConfig, to: GcalConfig): GcalConfig = {

    def diff[T](from: Option[T], to: Option[T])(implicit eq: Eq[T]): Option[T] = (from, to) match {
      case (Some(a), Some(b)) if a =!= b => Some(b)
      case (None, Some(b))               => Some(b)
      case _                             => None
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
    val lamps = config.extractAs[JSet[Lamp]](CALIBRATION_KEY / LAMP_PROP)
      .map(_.asScala.toList).recoverWithDefault(List.empty)

    val arLamp = lamps.map(v => if (v.contains(Lamp.AR_ARC)) Some(LampState.On) else Some(LampState.Off))
    val cuarLamp = lamps.map(v => if (v.contains(Lamp.CUAR_ARC)) Some(LampState.On) else Some(LampState.Off))
    val tharLamp = lamps.map(v => if (v.contains(Lamp.THAR_ARC)) Some(LampState.On) else Some(LampState.Off))
    val qhLamp = lamps.map(v => if (v.contains(Lamp.QUARTZ)) Some(LampState.On) else Some(LampState.Off))
    val xeLamp = lamps.map(v => if (v.contains(Lamp.XE_ARC)) Some(LampState.On) else Some(LampState.Off))
    val irLampCP = lamps.map(v => if (v.contains(Lamp.IR_GREY_BODY_HIGH) || v.contains(Lamp.IR_GREY_BODY_LOW)) Some(LampState.On) else None)
    val irLampMK = lamps.map(v => if (v.contains(Lamp.IR_GREY_BODY_HIGH)) Some(LampState.On)
                                  else if (v.contains(Lamp.IR_GREY_BODY_LOW)) Some(LampState.Off) else None)
    val shutter = config.extractAs[Shutter](CALIBRATION_KEY / SHUTTER_PROP).map(Some(_)).recoverWithDefault(None)
    val filter = config.extractAs[Filter](CALIBRATION_KEY / FILTER_PROP).map(Some(_)).recoverWithDefault(None)
    val diffuser = config.extractAs[Diffuser](CALIBRATION_KEY / DIFFUSER_PROP).map(Some(_)).recoverWithDefault(None)

    SeqAction.either(
      for {
        _    <- lamps
        ar   <- arLamp.map(_.map(ArLampState.apply))
        cuar <- cuarLamp.map(_.map(CuArLampState.apply))
        thar <- tharLamp.map(_.map(ThArLampState.apply))
        qh   <- qhLamp.map(_.map(QHLampState.apply))
        xe   <- xeLamp.map(_.map(XeLampState.apply))
        ir   <- (if (isCP) irLampCP else irLampMK).map(_.map(IrLampState.apply))
        sht  <- shutter
        flt  <- filter
        dif  <- diffuser
      } yield if(lamps.isEmpty && List[Option[_]](sht, flt, dif).forall(_.isEmpty)) GcalConfig.allOff
              else GcalConfig(ar, cuar, qh, thar, xe, ir, sht, flt, dif)
    )

  }
}
