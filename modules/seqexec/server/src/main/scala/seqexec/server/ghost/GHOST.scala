// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.data.Reader
import cats.data.EitherT
import cats.effect.Sync
import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.gemini.ghost.Ghost
import gem.math.{Angle, HourAngle}
import scala.concurrent.duration._
import seqexec.model.dhs.ImageFileId
import seqexec.model.Model.{Instrument, Resource}
import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.keywords.{GDSClient, GDSInstrument}
import seqexec.server.ghost.GHOSTController._
import squants.time.{Seconds, Time}

final case class GHOST[F[_]: Sync](controller: GHOSTController[F])
    extends InstrumentSystem[F]
    with GDSInstrument {
  override val gdsClient: GDSClient = controller.gdsClient

  override val resource: Resource = Instrument.GHOST

  override val sfName: String = "GHOST"

  override val contributorName: String = "ghost"

  override val observeControl: InstrumentSystem.ObserveControl =
    InstrumentSystem.Uncontrollable

  override def observe(
      config: Config): SeqObserveF[F, ImageFileId, ObserveCommand.Result] =
    Reader { fileId =>
      controller
        .observe(fileId)
        .map(_ => ObserveCommand.Success: ObserveCommand.Result)
    }

  override def configure(config: Config): SeqActionF[F, ConfigResult[F]] =
    GHOST
      .fromSequenceConfig[F](config)
      .flatMap(controller.applyConfig)
      .map(_ => ConfigResult[F](this))

  override def notifyObserveEnd: SeqActionF[F, Unit] = controller.endObserve

  override def notifyObserveStart: SeqActionF[F, Unit] = SeqActionF.void

  override def calcObserveTime(config: Config): Time = Seconds(360)
}

object GHOST {
  val INSTRUMENT_NAME_PROP: String = "GHOST"
  val name: String                 = INSTRUMENT_NAME_PROP

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

  def fromSequenceConfig[F[_]: Sync](
      config: Config): SeqActionF[F, GHOSTConfig] =
    EitherT(
      Sync[F].delay(
        (for {
          baseRAHMS          <- config
                                 .extract(INSTRUMENT_KEY / Ghost.BaseRAHMS)
                                 .as[Option[HourAngle]]
          baseDecDMS         <- config
                                 .extract(INSTRUMENT_KEY / Ghost.BaseDecDMS)
                                 .as[Option[Angle]]
          srifu1Name         <- config
                                 .extract(INSTRUMENT_KEY / Ghost.SRIFU1Name)
                                 .as[Option[String]]
          srifu1CoordsRAHMS  <- config
                                 .extract(INSTRUMENT_KEY / Ghost.SRIFU1RAHMS)
                                 .as[Option[HourAngle]]
          srifu1CoordsDecDMS <- config
                                 .extract(INSTRUMENT_KEY / Ghost.SRIFU1DecDMS)
                                 .as[Option[Angle]]
          srifu2Name         <- config
                                 .extract(INSTRUMENT_KEY / Ghost.SRIFU2Name)
                                 .as[Option[String]]
          srifu2CoordsRAHMS  <- config
                                 .extract(INSTRUMENT_KEY / Ghost.SRIFU2RAHMS)
                                 .as[Option[HourAngle]]
          srifu2CoordsDecDMS <- config
                                 .extract(INSTRUMENT_KEY / Ghost.SRIFU2DecDMS)
                                 .as[Option[Angle]]
          hrifu1Name         <- config
                                 .extract(INSTRUMENT_KEY / Ghost.HRIFU1Name)
                                 .as[Option[String]]
          hrifu1CoordsRAHMS  <- config
                                 .extract(INSTRUMENT_KEY / Ghost.HRIFU1RAHMS)
                                 .as[Option[HourAngle]]
          hrifu1CoordsDecDMS <- config
                                 .extract(INSTRUMENT_KEY / Ghost.HRIFU1DecDMS)
                                 .as[Option[Angle]]
          hrifu2CoordsRAHMS  <- config
                                 .extract(INSTRUMENT_KEY / Ghost.HRIFU2RAHMS)
                                 .as[Option[HourAngle]]
          hrifu2CoordsDecDMS <- config
                                 .extract(INSTRUMENT_KEY / Ghost.HRIFU2DecDMS)
                                 .as[Option[Angle]]
        } yield
          GHOSTConfig(
            baseRAHMS,
            baseDecDMS,
            1.minute,
            srifu1Name,
            srifu1CoordsRAHMS,
            srifu1CoordsDecDMS,
            srifu2Name,
            srifu2CoordsRAHMS,
            srifu2CoordsDecDMS,
            hrifu1Name,
            hrifu1CoordsRAHMS,
            hrifu1CoordsDecDMS,
            hrifu2CoordsRAHMS,
            hrifu2CoordsDecDMS
          ))
          .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      ))
}
