// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.igrins2

import cats.data.Kleisli
import cats.MonadError
import cats.effect.Sync
import cats.syntax.all._
import fs2.Stream
import edu.gemini.spModel.gemini.igrins2.{ Igrins2 => SPIgrins2 }
import java.lang.{ Double => JDouble }
import org.typelevel.log4cats.Logger
import lucuma.core.enums.LightSinkName
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server._
import seqexec.server.keywords.GdsClient
import seqexec.server.keywords.GdsInstrument
import seqexec.server.keywords.KeywordsClient
import squants.time.Seconds
import squants.time.Time
import cats.effect.Async
import seqexec.model.dhs.ImageFileId
import giapi.client.commands.Configuration
import squants.time.TimeConversions._
import seqexec.server.ConfigUtilOps._
import cats.data.EitherT

final case class Igrins2[F[_]: Logger: Async](
  controller: Igrins2Controller[F]
) extends GdsInstrument[F]
    with InstrumentSystem[F] {

  override val gdsClient: GdsClient[F] = controller.gdsClient

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Igrins2

  override val contributorName: String = "igrins2"

  val readoutOverhead: Time = Seconds(10)

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
    Igrins2
      .fromSequenceConfig[F](config)
      .flatMap(controller.applyConfig)
      .as(ConfigResult[F](this))

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  override def calcObserveTime(config: CleanConfig): F[Time] =
    MonadError[F, Throwable].catchNonFatal {
      val obsTime =
        for {
          exp <- config.extractObsAs[JDouble](SPIgrins2.EXPOSURE_TIME_PROP)
          t    = Seconds(exp.toDouble)
          f    = SPIgrins2.readoutTime(t)
        } yield t + f + readoutOverhead
      obsTime.getOrElse(readoutOverhead)
    }

  override def observeProgress(
    total:   Time,
    elapsed: InstrumentSystem.ElapsedTime
  ): Stream[F, Progress] =
    ProgressUtil.obsCountdown[F](total, elapsed.self)

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

}

object Igrins2 {
  val INSTRUMENT_NAME_PROP: String = "IGRINS2"
  val name: String                 = INSTRUMENT_NAME_PROP

  val sfName: String = "IGRINS2"

  def fromSequenceConfig[F[_]: Sync](config: CleanConfig): F[Igrins2Config] =
    EitherT {
      Sync[F].delay {
        (for {
          expTime       <-
            config.extractObsAs[JDouble](SPIgrins2.EXPOSURE_TIME_PROP).map(_.toDouble.seconds)
          igrins2Config <-
            Right(new Igrins2Config {
              override def configuration: Configuration =
                // TODO The ICD must indicate how to set the exposure time
                Configuration.single("igrin2:exposureTime", expTime.value)
            })
        } yield igrins2Config).leftMap(e => SeqexecFailure.Unexpected(ConfigUtilOps.explain(e)))
      }
    }.widenRethrowT

  object specifics extends InstrumentSpecifics {
    override val instrument: Instrument = Instrument.Igrins2

    override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Igrins2

  }
}
