// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.data.Kleisli
import cats.data.EitherT
import cats.effect.{Concurrent, Sync, Timer}
import cats.implicits._
import fs2.Stream
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.gemini.ghost.{Ghost => SPGhost}
import gem.enum.LightSinkName
import gsp.math.{Coordinates, Declination, RightAscension}
import gsp.math.optics.Format
import io.chrisdavenport.log4cats.Logger

import scala.concurrent.duration._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.CleanConfig.extractItem
import seqexec.server.keywords.GdsInstrument
import seqexec.server.keywords.GdsClient
import seqexec.server.keywords.KeywordsClient
import squants.time.Seconds
import squants.time.Time

import scala.reflect.ClassTag

final case class Ghost[F[_]: Logger: Concurrent: Timer](controller: GhostController[F])
    extends GdsInstrument[F]
    with InstrumentSystem[F] {

  override val gdsClient: GdsClient[F] = controller.gdsClient

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Ghost

  override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Ghost

  override val contributorName: String = "ghost"

  override def observeControl(config: CleanConfig): InstrumentSystem.ObserveControl[F] =
    InstrumentSystem.Uncontrollable

  override def observe(
    config: CleanConfig
  ): Kleisli[F, ImageFileId, ObserveCommandResult] =
    Kleisli { fileId =>
      calcObserveTime(config).flatMap { x =>
        controller
          .observe(fileId, x)
          .as(ObserveCommandResult.Success: ObserveCommandResult)
      }
    }

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    Ghost
      .fromSequenceConfig[F](config)
      .flatMap(controller.applyConfig)
      .as(ConfigResult[F](this))

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def calcObserveTime(config: CleanConfig): F[Time] = Seconds(360).pure[F]

  override def observeProgress(
    total:   Time,
    elapsed: InstrumentSystem.ElapsedTime): Stream[F, Progress] = Stream.empty

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

}

object Ghost {
  val INSTRUMENT_NAME_PROP: String = "GHOST"
  val name: String                 = INSTRUMENT_NAME_PROP

  val sfName: String = "GHOST"

  def fromSequenceConfig[F[_]: Sync](config: CleanConfig): F[GhostConfig] = {
    def extractor[A : ClassTag](propName: String): Option[A] =
      config.extractInstAs[A](propName).toOption

    def formatExtractor[A](fmt: Format[String, A]): String => Either[ExtractFailure, Option[A]] = { propName =>
      // 1. If content is None, trivial success, so Right(None).
      // 2. If processed content is Some(a), success, so Right(Some(content)).
      // 3. If processed content is None, failure, so Left(error).
      extractor[String](propName).map(fmt.getOption).map {
        case None => Left(ConversionError(INSTRUMENT_KEY / propName, s"Could not parse $propName"))
        case other => Right(other)
      }.getOrElse(Right(None))
    }

    val raExtractor = formatExtractor[RightAscension](RightAscension.fromStringHMS)
    val decExtractor = formatExtractor[Declination](Declination.fromStringSignedDMS)

    EitherT {
      Sync[F].delay {
        (for {
          baseRAHMS     <- raExtractor(SPGhost.BASE_RA_HMS)
          baseDecDMS    <- decExtractor(SPGhost.BASE_DEC_DMS)

          fiberAgitator = extractor[Boolean](SPGhost.FIBER_AGITATOR)
          srifu1Name    = extractor[String](SPGhost.SRIFU1_NAME)
          srifu1RAHMS   <- raExtractor(SPGhost.SRIFU1_RA_HMS)
          srifu1DecHDMS <- decExtractor(SPGhost.SRIFU1_DEC_DMS)

          srifu2Name    = extractor[String](SPGhost.SRIFU2_NAME)
          srifu2RAHMS   <- raExtractor(SPGhost.SRIFU2_RA_HMS)
          srifu2DecHDMS <- decExtractor(SPGhost.SRIFU2_DEC_DMS)

          hrifu1Name    = extractor[String](SPGhost.HRIFU1_NAME)
          hrifu1RAHMS   <- raExtractor(SPGhost.HRIFU1_RA_HMS)
          hrifu1DecHDMS <- decExtractor(SPGhost.HRIFU1_DEC_DMS)

          hrifu2RAHMS   <- raExtractor(SPGhost.HRIFU2_RA_HMS)
          hrifu2DecHDMS <- decExtractor(SPGhost.HRIFU2_DEC_DMS)

          config <- GhostConfig(
            (baseRAHMS, baseDecDMS).mapN(Coordinates.apply),
            1.minute,
            FiberAgitator.fromBoolean(fiberAgitator.getOrElse(false)),
            srifu1Name, (srifu1RAHMS, srifu1DecHDMS).mapN(Coordinates.apply),
            srifu2Name, (srifu2RAHMS, srifu2DecHDMS).mapN(Coordinates.apply),
            hrifu1Name, (hrifu1RAHMS, hrifu1DecHDMS).mapN(Coordinates.apply),
            hrifu2RAHMS.as("Sky"), (hrifu2RAHMS, hrifu2DecHDMS).mapN(Coordinates.apply))
        } yield {
          config
        }).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      }
    }.widenRethrowT
  }
}
