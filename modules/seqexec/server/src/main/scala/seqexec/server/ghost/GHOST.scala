// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import seqexec.model.dhs.ImageFileId
import seqexec.model.Model.{Instrument, Resource}

import edu.gemini.spModel.gemini.gpi.Gpi._
import cats.data.Reader
import squants.time.{Seconds, Time}

import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.ghost.GHOSTController._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import scala.concurrent.duration._

import cats.data.EitherT
import cats.effect.{IO, Sync}
import cats.implicits._
import gem.math.{Angle, HourAngle}

final case class GHOST[F[_]](controller: GHOSTController) extends InstrumentSystem {
  override val resource: Resource = Instrument.GHOST

  override val sfName: String = "GHOST"

  override val contributorName: String = "ghost"

  override val dhsInstrumentName: String = "GHOST"

  override val observeControl: InstrumentSystem.ObserveControl =
    InstrumentSystem.Uncontrollable

  // TODO: No idea what the second parameter to observe should be.
  override def observe(
                        config: Config): SeqObserve[ImageFileId, ObserveCommand.Result] = Reader {
    fileId =>
      controller.observe(fileId, Seconds(60)).map(_ => ObserveCommand.Success)
  }

  override def configure(config: Config): SeqAction[ConfigResult] =
    GHOST
      .fromSequenceConfig[IO](config)
      .flatMap(controller.applyConfig)
      .map(_ => ConfigResult(this))

  override def notifyObserveEnd: SeqAction[Unit] = controller.endObserve

  override def calcObserveTime(config: Config): Time =
    config
      .extract(OBSERVE_KEY / EXPOSURE_TIME_PROP)
      .as[java.lang.Double]
      .map(x => Seconds(x.toDouble))
      .getOrElse(Seconds(360))
}

object GHOST {
  val INSTRUMENT_NAME_PROP: String = "GHOST"
  val name: String = INSTRUMENT_NAME_PROP

  val sfName: String = "GHOST"

  // TODO: I am sure there is a cleaner way to write this without using for, but I'm not sure right
  // TODO: now what it is.
//  private def hmsAngle(config: Config, name: String): Either[ExtractFailure, Option[HourAngle]] =
//    for {
//      angOptStr <- config.extract(INSTRUMENT_KEY / name).as[Option[String]]
//      angStr    <- angOptStr
//      ang       <- HourAngle.fromStringHMS.getOption(angStr)
//    } yield ang
//  private def hmsAngle(config: Config, name: String): Either[ExtractFailure, Option[HourAngle]] =
//    for {
//      a <- config.extract(INSTRUMENT_KEY / name).as[String]
//          .map(HourAngle.fromStringHMS.getOption)
//    } yield a
//
//  private def dmsAngle(config: Config, name: String): Either[ExtractFailure, Option[Angle]] =
//    for {
//      a <- config.extract(INSTRUMENT_KEY / name).as[String]
//        .map(Angle.fromStringSignedDMS.getOption)
//    } yield a
//
//
//  private def targetName(config: Config, name: String): Either[ExtractFailure, Option[String]] =
//    config.extract(INSTRUMENT_KEY / name).as[Option[String]]

  // TODO: Obviously, these will not be hard-coded strings in the final version.
  // TODO: They are in Ghost.scala in OCS2.
  // TODO: Also, we are skipping degree strings here as we don't want to have to deal with them.
  // TODO: Placeholder for exposure time here.
  def fromSequenceConfig[F[_]: Sync](config: Config): SeqActionF[F, GHOSTConfig] =
    EitherT(Sync[F].delay(
      (for {
        baseRAHMS          <- config.extract(INSTRUMENT_KEY / "baseRAHMS").as[Option[HourAngle]]
        baseDecDMS         <- config.extract(INSTRUMENT_KEY / "baseDecDMS").as[Option[Angle]]
        srifu1Name         <- config.extract(INSTRUMENT_KEY / "srifu1Name").as[Option[String]]
        srifu1CoordsRAHMS  <- config.extract(INSTRUMENT_KEY / "srifu1CoordsRAHMS").as[Option[HourAngle]]
        srifu1CoordsDecDMS <- config.extract(INSTRUMENT_KEY / "srifu1CoordsDecDMS").as[Option[Angle]]
        srifu2Name         <- config.extract(INSTRUMENT_KEY / "srifu2Name").as[Option[String]]
        srifu2CoordsRAHMS  <- config.extract(INSTRUMENT_KEY / "srifu2CoordsRAHMS").as[Option[HourAngle]]
        srifu2CoordsDecDMS <- config.extract(INSTRUMENT_KEY / "srifu2CoordsDecDMS").as[Option[Angle]]
        hrifu1Name         <- config.extract(INSTRUMENT_KEY / "hrifu1Name").as[Option[String]]
        hrifu1CoordsRAHMS  <- config.extract(INSTRUMENT_KEY / "hrifu1CoordsRAHMS").as[Option[HourAngle]]
        hrifu1CoordsDecDMS <- config.extract(INSTRUMENT_KEY / "hrifu1CoordsDecDMS").as[Option[Angle]]
        hrifu2CoordsRAHMS  <- config.extract(INSTRUMENT_KEY / "hrifu2CoordsRAHMS").as[Option[HourAngle]]
        hrifu2CoordsDecDMS <- config.extract(INSTRUMENT_KEY / "hrifu2CoordsDecDMS").as[Option[Angle]]
      } yield GHOSTConfig(baseRAHMS, baseDecDMS, 1.minute,
                          srifu1Name, srifu1CoordsRAHMS, srifu1CoordsDecDMS,
                          srifu2Name, srifu2CoordsRAHMS, srifu2CoordsDecDMS,
                          hrifu1Name, hrifu1CoordsRAHMS, hrifu1CoordsDecDMS,
                          hrifu2CoordsRAHMS, hrifu2CoordsDecDMS))
        .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
    ))
}
