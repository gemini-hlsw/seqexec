// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.Sync
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import edu.gemini.spModel.gemini.ghost.{ Ghost => SPGhost }
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.obscomp.InstConstants.OBSERVE_TYPE_PROP
import edu.gemini.spModel.obscomp.InstConstants.SCIENCE_OBSERVE_TYPE
import edu.gemini.spModel.gemini.ghost.GhostReadNoiseGain
import fs2.Stream
import org.typelevel.log4cats.Logger
import lucuma.core.enums.LightSinkName
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.optics.Format
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.server._
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps._
import seqexec.server.keywords.GdsClient
import seqexec.server.keywords.GdsInstrument
import seqexec.server.keywords.KeywordsClient
import squants.time.Time
import scala.reflect.ClassTag
import lucuma.core.math._
import lucuma.core.model._
import java.lang.{ Double => JDouble, Integer => JInt }
import edu.gemini.spModel.gemini.ghost.GhostBinning
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import squants.time.Milliseconds
import squants.time.Seconds
import squants.time.Minutes
import cats.effect.Async

final case class Ghost[F[_]: Logger: Async](controller: GhostController[F])
    extends GdsInstrument[F]
    with InstrumentSystem[F] {

  // Needs to be estimated experimentally
  val readoutOverhead: Time = Seconds(30)

  override val gdsClient: GdsClient[F] = controller.gdsClient

  override val keywordsClient: KeywordsClient[F] = this

  override val resource: Instrument = Instrument.Ghost

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

  override def calcObserveTime(config: CleanConfig): F[Time] = {
    val ghostConfig = Ghost.fromSequenceConfig[F](config)
    ghostConfig.map(c =>
      if (!c.isScience) Minutes(6) // we can't yet calculate how long a bias takes
      else
        readoutOverhead + Milliseconds(
          (c.blueConfig.exposure * c.blueConfig.count.toLong)
            .max(c.redConfig.exposure * c.redConfig.count.toLong)
            .toMillis
        )
    )
  }

  override def observeProgress(
    total:   Time,
    elapsed: InstrumentSystem.ElapsedTime
  ): Stream[F, Progress] =
    ProgressUtil.obsCountdown[F](total, elapsed.self)

  override def instrumentActions(config: CleanConfig): InstrumentActions[F] =
    InstrumentActions.defaultInstrumentActions[F]

}

object Ghost {
  val INSTRUMENT_NAME_PROP: String = "GHOST"
  val name: String                 = INSTRUMENT_NAME_PROP

  val sfName: String = "GHOST"

  def fromSequenceConfig[F[_]: Sync](config: CleanConfig): F[GhostConfig] = {
    def extractor[A: ClassTag](propName: String): Option[A] =
      config.extractInstAs[A](propName).toOption

    def formatExtractor[A](fmt: Format[String, A]): String => Either[ExtractFailure, Option[A]] = {
      propName =>
        // 1. If content is None, trivial success, so Right(None).
        // 2. If processed content is Some(a), success, so Right(Some(content)).
        // 3. If processed content is None, failure, so Left(error).
        extractor[String](propName)
          .map(fmt.getOption)
          .map {
            case None  =>
              Left(ConversionError(INSTRUMENT_KEY / propName, s"Could not parse $propName"))
            case other => Right(other)
          }
          .getOrElse(Right(None))
    }

    val raExtractor  = formatExtractor[RightAscension](RightAscension.fromStringHMS)
    val decExtractor = formatExtractor[Declination](Declination.fromStringSignedDMS)

    val MaxTargets = 8

    def gainFromODB(n: GhostReadNoiseGain): ReadNoiseGain = n match {
      case GhostReadNoiseGain.SLOW_LOW  => ReadNoiseGain.SlowLow
      case GhostReadNoiseGain.FAST_LOW  => ReadNoiseGain.FastLow
      case GhostReadNoiseGain.FAST_HIGH => ReadNoiseGain.FastHigh
    }

    def userTargets: List[Option[Target]] = (for {
      i <- 1 to MaxTargets
    } yield {
      val (a, _, _, d, e) = SPGhost.userTargetParams(i)
      (for {
        ra  <- raExtractor(d)
        dec <- decExtractor(e)
        c    = (ra, dec).mapN(Coordinates.apply)
        n   <- config.extractInstAs[String](a).flatMap(refineV[NonEmpty](_))
        // Note the coordinates are PM corrected on the OT side
      } yield c.map(coord =>
        Target(n,
               SiderealTracking(none, coord, Epoch.J2000, none, none, none).asRight,
               SortedMap.empty[MagnitudeBand, Magnitude]
        )
      )).toOption.flatten
    }).toList

    EitherT {
      Sync[F].delay {
        (for {
          baseRAHMS  <- raExtractor(SPGhost.BASE_RA_HMS)
          baseDecDMS <- decExtractor(SPGhost.BASE_DEC_DMS)

          fiberAgitator1 = extractor[Boolean](SPGhost.FIBER_AGITATOR_1)
          fiberAgitator2 = extractor[Boolean](SPGhost.FIBER_AGITATOR_2)
          srifu1Name     = extractor[String](SPGhost.SRIFU1_NAME)
          srifu1RAHMS   <- raExtractor(SPGhost.SRIFU1_RA_HMS)
          srifu1DecHDMS <- decExtractor(SPGhost.SRIFU1_DEC_DMS)

          srifu2Name     = extractor[String](SPGhost.SRIFU2_NAME)
          srifu2RAHMS   <- raExtractor(SPGhost.SRIFU2_RA_HMS)
          srifu2DecHDMS <- decExtractor(SPGhost.SRIFU2_DEC_DMS)

          hrifu1Name     = extractor[String](SPGhost.HRIFU1_NAME)
          hrifu1RAHMS   <- raExtractor(SPGhost.HRIFU1_RA_HMS)
          hrifu1DecHDMS <- decExtractor(SPGhost.HRIFU1_DEC_DMS)

          hrifu2RAHMS   <- raExtractor(SPGhost.HRIFU2_RA_HMS)
          hrifu2DecHDMS <- decExtractor(SPGhost.HRIFU2_DEC_DMS)
          obsType       <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
          science        = obsType === SCIENCE_OBSERVE_TYPE

          blueBinning  <- config.extractInstAs[GhostBinning](SPGhost.BLUE_BINNING_PROP)
          redBinning   <- config.extractInstAs[GhostBinning](SPGhost.RED_BINNING_PROP)
          blueExposure <-
            config.extractObsAs[JDouble](SPGhost.BLUE_EXPOSURE_TIME_PROP).map(_.doubleValue())
          redExposure  <-
            config.extractObsAs[JDouble](SPGhost.RED_EXPOSURE_TIME_PROP).map(_.doubleValue())
          blueCount    <- config.extractObsAs[JInt](SPGhost.BLUE_EXPOSURE_COUNT_PROP).map(_.intValue())
          blueReadMode <-
            config
              .extractInstAs[GhostReadNoiseGain](SPGhost.BLUE_READ_NOISE_GAIN_PROP)
          redCount     <- config.extractObsAs[JInt](SPGhost.RED_EXPOSURE_COUNT_PROP).map(_.intValue())
          redReadMode  <-
            config
              .extractInstAs[GhostReadNoiseGain](SPGhost.BLUE_READ_NOISE_GAIN_PROP)

          config <- {
            if (science) {
              GhostConfig.apply(
                obsType = obsType,
                blueConfig = ChannelConfig(blueBinning,
                                           blueExposure.second,
                                           blueCount,
                                           gainFromODB(blueReadMode)
                ),
                redConfig =
                  ChannelConfig(redBinning, redExposure.second, redCount, gainFromODB(redReadMode)),
                baseCoords = (baseRAHMS, baseDecDMS).mapN(Coordinates.apply),
                fiberAgitator1 = FiberAgitator.fromBoolean(fiberAgitator1.getOrElse(false)),
                fiberAgitator2 = FiberAgitator.fromBoolean(fiberAgitator2.getOrElse(false)),
                srifu1Name = srifu1Name,
                srifu1Coords = (srifu1RAHMS, srifu1DecHDMS).mapN(Coordinates.apply),
                srifu2Name = srifu2Name,
                srifu2Coords = (srifu2RAHMS, srifu2DecHDMS).mapN(Coordinates.apply),
                hrifu1Name = hrifu1Name,
                hrifu1Coords = (hrifu1RAHMS, hrifu1DecHDMS).mapN(Coordinates.apply),
                hrifu2Name = hrifu2RAHMS.as("Sky"),
                hrifu2Coords = (hrifu2RAHMS, hrifu2DecHDMS).mapN(Coordinates.apply),
                userTargets = userTargets.flatten
              )
            } else
              GhostCalibration(
                obsType = obsType,
                blueConfig = ChannelConfig(blueBinning,
                                           blueExposure.second,
                                           blueCount,
                                           gainFromODB(blueReadMode)
                ),
                redConfig =
                  ChannelConfig(redBinning, redExposure.second, redCount, gainFromODB(redReadMode)),
                baseCoords = (baseRAHMS, baseDecDMS).mapN(Coordinates.apply),
                fiberAgitator1 = FiberAgitator.fromBoolean(fiberAgitator1.getOrElse(false)),
                fiberAgitator2 = FiberAgitator.fromBoolean(fiberAgitator2.getOrElse(false))
              ).asRight
          }
        } yield config).leftMap { e =>
          System.out.println(e); SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))
        }
      }
    }.widenRethrowT
  }

  object specifics extends InstrumentSpecifics {
    override val instrument: Instrument = Instrument.Ghost

    override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Ghost

  }
}
