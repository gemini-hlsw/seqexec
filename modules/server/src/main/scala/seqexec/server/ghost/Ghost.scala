// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import cats.data.EitherT
import cats.data.Kleisli
import cats.effect.Sync
import cats.effect.Async
import cats.effect.Ref
import cats.syntax.all._
import eu.timepit.refined._
import eu.timepit.refined.collection.NonEmpty
import edu.gemini.spModel.gemini.ghost.{ Ghost => SPGhost }
import edu.gemini.spModel.seqcomp.SeqConfigNames._
import edu.gemini.spModel.obscomp.InstConstants.OBS_CLASS_PROP
import edu.gemini.spModel.obscomp.InstConstants.OBSERVE_TYPE_PROP
import edu.gemini.spModel.obscomp.InstConstants.SCIENCE_OBSERVE_TYPE
import edu.gemini.spModel.gemini.ghost.GhostReadNoiseGain
import fs2.Stream
import org.typelevel.log4cats.Logger
import lucuma.core.enums.LightSinkName
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.UnnormalizedSED
import lucuma.core.optics.Format
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Instrument
import seqexec.model.enum.ObserveCommandResult
import seqexec.model.Conditions
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
import java.lang.{ Boolean => JBoolean, Double => JDouble, Integer => JInt }
import edu.gemini.spModel.gemini.ghost.GhostBinning
import scala.collection.immutable.SortedMap
import scala.concurrent.duration._
import squants.time.Milliseconds
import squants.time.Minutes
import lucuma.core.enums.StellarLibrarySpectrum
import edu.gemini.spModel.target.env.ResolutionMode

final case class Ghost[F[_]: Logger: Async](
  controller: GhostController[F],
  conditions: Ref[F, Conditions]
) extends GdsInstrument[F]
    with InstrumentSystem[F]
    with GhostLUT {
  // Readout time to fallback
  val fallbackReadouTimeRed: Duration = ReadoutTimesLUT.map(_.readRed).max

  val fallbackReadouTimeBlue: Duration = ReadoutTimesLUT.map(_.readBlue).max

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
    for {
      cond <- conditions.get
      cfg  <- Ghost.fromSequenceConfig[F](config, cond)
      _    <- controller.applyConfig(cfg)
    } yield ConfigResult[F](this)

  override def notifyObserveEnd: F[Unit] =
    controller.endObserve

  override def notifyObserveStart: F[Unit] = Sync[F].unit

  // REL-4239
  private def totalObserveTime(config: GhostConfig): Time = {
    val blueKey   =
      (config.blueConfig.readMode, config.blueConfig.binning)
    val blue      = ReadoutTimesLUT
      .find(x => (x.mode, x.binning) == blueKey)
      .map(_.readBlue)
      .getOrElse(fallbackReadouTimeBlue)
    val redKey    =
      (config.redConfig.readMode, config.redConfig.binning)
    val red       = ReadoutTimesLUT
      .find(x => (x.mode, x.binning) == redKey)
      .map(_.readRed)
      .getOrElse(fallbackReadouTimeRed)
    val blueTotal = config.blueConfig.count.toLong * (config.blueConfig.exposure + blue)
    val redTotal  = config.redConfig.count.toLong * (config.redConfig.exposure + red)

    Milliseconds(blueTotal.max(redTotal).toMillis)
  }

  override def calcObserveTime(config: CleanConfig): F[Time] = {
    val ghostConfig = conditions.get.flatMap(Ghost.fromSequenceConfig[F](config, _))
    ghostConfig.map(c =>
      totalObserveTime(c)
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

trait GhostConfigUtil {
  val INSTRUMENT_NAME_PROP: String = "GHOST"
  val name: String                 = INSTRUMENT_NAME_PROP

  def extractor[A: ClassTag](config: CleanConfig, propName: String): Option[A] =
    config.extractInstAs[A](propName).toOption

  def formatExtractor[A](
    config: CleanConfig,
    fmt:    Format[String, A]
  ): String => Either[ExtractFailure, Option[A]] = { propName =>
    // 1. If content is None, trivial success, so Right(None).
    // 2. If processed content is Some(a), success, so Right(Some(content)).
    // 3. If processed content is None, failure, so Left(error).
    extractor[String](config, propName)
      .map(fmt.getOption)
      .map {
        case None  =>
          Left(ConversionError(INSTRUMENT_KEY / propName, s"Could not parse $propName"))
        case other => Right(other)
      }
      .getOrElse(Right(None))
  }

  def raExtractorBase(config: CleanConfig) =
    formatExtractor[RightAscension](config, RightAscension.fromStringHMS)

  def decExtractorBase(config: CleanConfig) =
    formatExtractor[Declination](config, Declination.fromStringSignedDMS)

}

object Ghost extends GhostConfigUtil {

  val sfName: String = "GHOST"

  def fromSequenceConfig[F[_]: Sync](
    config:     CleanConfig,
    conditions: Conditions
  ): F[GhostConfig] = {
    val raExtractor  = raExtractorBase(config)
    val decExtractor = decExtractorBase(config)

    val MaxTargets = 8

    def gainFromODB(n: GhostReadNoiseGain): ReadNoiseGain = n match {
      case GhostReadNoiseGain.SLOW_LOW   => ReadNoiseGain.Slow
      case GhostReadNoiseGain.MEDIUM_LOW => ReadNoiseGain.Medium
      case GhostReadNoiseGain.FAST_LOW   => ReadNoiseGain.Fast
      case GhostReadNoiseGain.FAST_HIGH  => ReadNoiseGain.Fast
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
        Target.Sidereal(
          n,
          SiderealTracking(coord, Epoch.J2000, none, none, none),
          SourceProfile.Point(
            SpectralDefinition.BandNormalized(
              UnnormalizedSED.StellarLibrary(StellarLibrarySpectrum.A0V),
              SortedMap.empty
            )
          ),
          None
        )
      )).toOption.flatten
    }).toList

    EitherT {
      Sync[F].delay {
        (for {
          baseRAHMS  <- raExtractor(SPGhost.BASE_RA_HMS)
          baseDecDMS <- decExtractor(SPGhost.BASE_DEC_DMS)

          fiberAgitator1 =
            config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_1).map(_.booleanValue())
          fiberAgitator2 =
            config.extractInstAs[JBoolean](SPGhost.FIBER_AGITATOR_2).map(_.booleanValue())
          srifu1Name     = extractor[String](config, SPGhost.SRIFU1_NAME)
          srifu1RAHMS   <- raExtractor(SPGhost.SRIFU1_RA_HMS)
          srifu1DecHDMS <- decExtractor(SPGhost.SRIFU1_DEC_DMS)

          srifu2Name     = extractor[String](config, SPGhost.SRIFU2_NAME)
          srifu2RAHMS   <- raExtractor(SPGhost.SRIFU2_RA_HMS)
          srifu2DecHDMS <- decExtractor(SPGhost.SRIFU2_DEC_DMS)

          hrifu1Name     = extractor[String](config, SPGhost.HRIFU1_NAME)
          hrifu1RAHMS   <- raExtractor(SPGhost.HRIFU1_RA_HMS)
          hrifu1DecHDMS <- decExtractor(SPGhost.HRIFU1_DEC_DMS)

          hrifu2RAHMS   <- raExtractor(SPGhost.HRIFU2_RA_HMS)
          hrifu2DecHDMS <- decExtractor(SPGhost.HRIFU2_DEC_DMS)
          obsClass      <- config.extractObsAs[String](OBS_CLASS_PROP)
          obsType       <- config.extractObsAs[String](OBSERVE_TYPE_PROP)
          isScience      = obsType === SCIENCE_OBSERVE_TYPE

          blueBinning  <- config.extractInstAs[GhostBinning](SPGhost.BLUE_BINNING_PROP)
          redBinning   <- config.extractInstAs[GhostBinning](SPGhost.RED_BINNING_PROP)
          blueExposure <-
            config.extractObsAs[JDouble](SPGhost.BLUE_EXPOSURE_TIME_PROP).map(_.doubleValue())
          redExposure  <-
            config.extractObsAs[JDouble](SPGhost.RED_EXPOSURE_TIME_PROP).map(_.doubleValue())
          blueCount    <- config.extractObsAs[JInt](SPGhost.BLUE_EXPOSURE_COUNT_PROP).map(_.intValue())
          redCount     <- config.extractObsAs[JInt](SPGhost.RED_EXPOSURE_COUNT_PROP).map(_.intValue())
          blueReadMode <-
            config
              .extractInstAs[GhostReadNoiseGain](SPGhost.BLUE_READ_NOISE_GAIN_PROP)
          redReadMode  <-
            config
              .extractInstAs[GhostReadNoiseGain](SPGhost.RED_READ_NOISE_GAIN_PROP)
          rm            =
            config
              .extractInstAs[ResolutionMode](SPGhost.RESOLUTION_MODE)
          vMag         <-
            config
              .extractInstAs[JDouble](SPGhost.MAG_V_PROP)
              .map(_.doubleValue().some)
              .recoverWith(_ => none.asRight)

          gMag   <-
            config
              .extractInstAs[JDouble](SPGhost.MAG_G_PROP)
              .map(_.doubleValue().some)
              .recoverWith(_ => none.asRight)
          config <- {
            if (isScience && rm.toOption.isEmpty) {
              Left(ContentError("Science steps need a resolution mode defined"))
            } else {
              println(obsType)
              if (isScience) {
                GhostConfig.apply(
                  obsType = obsType,
                  obsClass = obsClass,
                  blueConfig = ChannelConfig(blueBinning,
                                             blueExposure.second,
                                             blueCount,
                                             gainFromODB(blueReadMode)
                  ),
                  redConfig = ChannelConfig(redBinning,
                                            redExposure.second,
                                            redCount,
                                            gainFromODB(redReadMode)
                  ),
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
                  userTargets = userTargets.flatten,
                  rm.toOption,
                  conditions,
                  vMag.orElse(gMag)
                )
              } else
                GhostCalibration(
                  obsType = obsType,
                  obsClass = obsClass,
                  blueConfig = ChannelConfig(blueBinning,
                                             blueExposure.second,
                                             blueCount,
                                             gainFromODB(blueReadMode)
                  ),
                  redConfig = ChannelConfig(redBinning,
                                            redExposure.second,
                                            redCount,
                                            gainFromODB(redReadMode)
                  ),
                  baseCoords = (baseRAHMS, baseDecDMS).mapN(Coordinates.apply),
                  fiberAgitator1 = FiberAgitator.fromBoolean(fiberAgitator1.getOrElse(false)),
                  fiberAgitator2 = FiberAgitator.fromBoolean(fiberAgitator2.getOrElse(false)),
                  rm.toOption,
                  conditions
                ).asRight
            }
          }
        } yield config).leftMap { e =>
          SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))
        }
      }
    }.widenRethrowT
  }

  object specifics extends InstrumentSpecifics {
    override val instrument: Instrument = Instrument.Ghost

    override def sfName(config: CleanConfig): LightSinkName = LightSinkName.Ghost

  }
}
