// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.data.Reader
import cats.data.EitherT
import cats.effect.{IO, Sync}
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.gemini.ghost.Ghost
import gem.math.{Angle, HourAngle}

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
}

object GHOST {
  val INSTRUMENT_NAME_PROP: String = "GHOST"
  val name: String                 = INSTRUMENT_NAME_PROP

  val sfName: String = "GHOST"

  // We always want a GHOSTConfig at this point, so don't use a for comprehension as not all parameters will be
  // present and we don't want the for to bork prematurely with a missing key error.
  def fromSequenceConfig[F[_]: Sync](config: Config): SeqActionF[F, GHOSTConfig] = {
    def extractor[A : ClassTag](propName: String): Option[A] =
      config.extractAs[A](INSTRUMENT_KEY / propName).toOption

    EitherT {
      Sync[F].delay {
        // This has the shortcoming that a failure in HourAngle or Angle parsing will result in a None instead
        // of in a SeqexecFailure, but for this preliminary phase, don't worry about it, as this is just for
        // basic testing.
        val baseRAHMS          = extractor[HourAngle](Ghost.BaseRAHMS)
        val baseDecDMS         = extractor[Angle    ](Ghost.BaseDecDMS)

        val srifu1Name         = extractor[String   ](Ghost.SRIFU1Name)
        val srifu1CoordsRAHMS  = extractor[HourAngle](Ghost.SRIFU1RAHMS)
        val srifu1CoordsDecDMS = extractor[Angle    ](Ghost.SRIFU1DecDMS)

        val srifu2Name         = extractor[String   ](Ghost.SRIFU2Name)
        val srifu2CoordsRAHMS  = extractor[HourAngle](Ghost.SRIFU2RAHMS)
        val srifu2CoordsDecDMS = extractor[Angle    ](Ghost.SRIFU2DecDMS)

        val hrifu1Name         = extractor[String   ](Ghost.HRIFU1Name)
        val hrifu1CoordsRAHMS  = extractor[HourAngle](Ghost.HRIFU1RAHMS)
        val hrifu1CoordsDecDMS = extractor[Angle    ](Ghost.HRIFU1DecDMS)

        val hrifu2CoordsRAHMS  = extractor[HourAngle](Ghost.HRIFU2RAHMS)
        val hrifu2CoordsDecDMS = extractor[Angle    ](Ghost.HRIFU2DecDMS)

        Right(GHOSTConfig(
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
        )): Either[SeqexecFailure, GHOSTConfig]
      }
    }
  }
}
