// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.data.Reader
import cats.data.EitherT
import cats.effect.{IO, Sync}
import cats.implicits._
import fs2.Stream
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.gemini.ghost.Ghost
import gem.math.{Angle, HourAngle}
import gem.optics.Format

import scala.concurrent.duration._
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.{Instrument, Resource}
import seqexec.server.ConfigUtilOps._
import seqexec.server._
import seqexec.server.keywords.{GDSClient, GDSInstrument, KeywordsClient}
import seqexec.server.ghost.GHOSTController._
import squants.time.{Seconds, Time}

import scala.reflect.ClassTag

final case class GHOST[F[_]: Sync](controller: GHOSTController[F])
    extends InstrumentSystem[F]
    with GDSInstrument {
  override val gdsClient: GDSClient = controller.gdsClient

  override val keywordsClient: KeywordsClient[IO] = this

  override val resource: Resource = Instrument.GHOST

  override val sfName: String = "GHOST"

  override val contributorName: String = "ghost"

  override val observeControl: InstrumentSystem.ObserveControl =
    InstrumentSystem.Uncontrollable

  override def observe(
      config: Config): SeqObserveF[F, ImageFileId, ObserveCommand.Result] =
    Reader { fileId =>
      controller
        .observe(fileId, calcObserveTime(config))
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

  override def observeProgress(total: Time, elapsed: InstrumentSystem.ElapsedTime): Stream[F, Progress] = Stream.empty
}

object GHOST {
  val INSTRUMENT_NAME_PROP: String = "GHOST"
  val name: String                 = INSTRUMENT_NAME_PROP

  val sfName: String = "GHOST"

  def fromSequenceConfig[F[_]: Sync](config: Config): SeqActionF[F, GHOSTConfig] = {
    def extractor[A : ClassTag](propName: String): Option[A] =
      config.extractAs[A](INSTRUMENT_KEY / propName).toOption

    def formatExtractor[A](fmt: Format[String, A]): String => Either[ExtractFailure, Option[A]] = { propName =>
      // 1. If content is None, trivial success, so Right(None).
      // 2. If processed content is Some(a), success, so Right(Some(content)).
      // 3. If processed content is None, failure, so Left(error).
      extractor[String](propName).map(fmt.getOption).map {
        case None => Left(ConversionError(INSTRUMENT_KEY / propName, s"Could not parse $propName"))
        case other => Right(other)
      }.getOrElse(Right(None))
    }

    val raExtractor = formatExtractor[HourAngle](HourAngle.fromStringHMS)
    val decExtractor = formatExtractor[Angle](Angle.fromStringDMS)

    EitherT {
      Sync[F].delay {
        (for {
          baseRAHMS     <- raExtractor(Ghost.BaseRAHMS)
          baseDecDMS    <- decExtractor(Ghost.BaseDecDMS)

          srifu1Name    = extractor[String](Ghost.SRIFU1Name)
          srifu1RAHMS   <- raExtractor(Ghost.SRIFU1RAHMS)
          srifu1DecHDMS <- decExtractor(Ghost.SRIFU1DecDMS)

          srifu2Name    = extractor[String](Ghost.SRIFU2Name)
          srifu2RAHMS   <- raExtractor(Ghost.SRIFU2RAHMS)
          srifu2DecHDMS <- decExtractor(Ghost.SRIFU2DecDMS)

          hrifu1Name    = extractor[String](Ghost.HRIFU1Name)
          hrifu1RAHMS   <- raExtractor(Ghost.HRIFU1RAHMS)
          hrifu1DecHDMS <- decExtractor(Ghost.HRIFU1DecDMS)

          hrifu2RAHMS   <- raExtractor(Ghost.HRIFU2RAHMS)
          hrifu2DecHDMS <- decExtractor(Ghost.HRIFU2DecDMS)

        } yield {
          val hrifu2Name = hrifu2RAHMS.as("Sky")
          GHOSTConfig(
            baseRAHMS, baseDecDMS, 1.minute,
            srifu1Name, srifu1RAHMS, srifu1DecHDMS,
            srifu2Name, srifu2RAHMS, srifu2DecHDMS,
            hrifu1Name, hrifu1RAHMS, hrifu1DecHDMS,
            hrifu2Name, hrifu2RAHMS, hrifu2DecHDMS)}
          )
          .leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      }
    }
  }
}
