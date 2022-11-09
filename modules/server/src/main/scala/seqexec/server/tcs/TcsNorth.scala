// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import cats.syntax.all._
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import org.typelevel.log4cats.Logger
import monocle.macros.Lenses
import mouse.all._
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.Resource
import seqexec.server.CleanConfig
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigResult
import seqexec.server.ConfigUtilOps._
import seqexec.server.InstrumentGuide
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.{ AltairConfig, AltairOff }
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsNorthController.TcsNorthAoConfig
import seqexec.server.tcs.TcsNorthController.TcsNorthConfig
import shapeless.tag
import shapeless.tag.@@
import squants.Angle
import squants.space.Arcseconds

class TcsNorth[F[_]: Sync: Logger] private (
  tcsController: TcsNorthController[F],
  subsystems:    NonEmptySet[Subsystem],
  gaos:          Option[Altair[F]],
  guideDb:       GuideConfigDb[F]
)(config:        TcsNorth.TcsSeqConfig[F])
    extends Tcs[F] {
  import Tcs.GuideWithOps

  val Log: Logger[F] = Logger[F]

  override val resource: Resource = Resource.TCS

  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsNorthConfig, subsystem: Subsystem): String =
    (subsystem match {
      case Subsystem.M1     => pprint.apply(tcs.gc.m1Guide)
      case Subsystem.M2     => pprint.apply(tcs.gc.m2Guide)
      case Subsystem.OIWFS  => pprint.apply(tcs.gds.oiwfs)
      case Subsystem.PWFS1  => pprint.apply(tcs.gds.pwfs1)
      case Subsystem.PWFS2  => pprint.apply(tcs.gds.pwfs2)
      case Subsystem.Mount  => pprint.apply(tcs.tc)
      case Subsystem.AGUnit => pprint.apply(List(tcs.agc.sfPos, tcs.agc.hrwfs))
      case Subsystem.Gaos   =>
        tcs match {
          case x: TcsNorthAoConfig => pprint.apply(x.gds.aoguide)
          case _                   => pprint.apply("")
        }
    }).plainText

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    buildTcsConfig.flatMap { cfg =>
      subsystems.traverse_(s =>
        Log.debug(s"Applying TCS/$s configuration/config: ${subsystemConfig(cfg, s)}")
      ) *>
        tcsController.applyConfig(subsystems, gaos, cfg).as(ConfigResult(this))
    }

  override def notifyObserveStart: F[Unit] = tcsController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = tcsController.notifyObserveEnd

  val defaultGuiderConf: GuiderConfig = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
  def calcGuiderConfig(
    guideWith: Option[StandardGuideOptions.Value]
  ): GuiderConfig =
    guideWith
      .map(v => GuiderConfig(v.toProbeTracking, v.toGuideSensorOption))
      .getOrElse(defaultGuiderConf)

  /*
   * Build TCS configuration for the step, merging the guide configuration from the sequence with the guide
   * configuration set from TCC. The TCC configuration has precedence: if a guider is not used in the TCC configuration,
   * it will not be used for the step, regardless of the sequence values.
   */
  private def buildBasicTcsConfig(gc: GuideConfig): TcsNorthConfig =
    BasicTcsConfig(
      gc.tcsGuide,
      TelescopeConfig(config.offsetA, config.wavelA),
      BasicGuidersConfig(
        tag[P1Config](
          calcGuiderConfig(config.guideWithP1)
        ),
        tag[P2Config](
          calcGuiderConfig(config.guideWithP2)
        ),
        tag[OIConfig](
          calcGuiderConfig(config.guideWithOI)
        )
      ),
      AGConfig(config.lightPath, HrwfsConfig.Auto.some),
      config.instrument
    )

  private def buildTcsAoConfig(gc: GuideConfig, ao: Altair[F]): TcsNorthConfig = {
    val aog = gc.gaosGuide.flatMap(_.swap.toOption).getOrElse(AltairOff)

    val aoGuiderConfig = ao
      .hasTarget(aog)
      .fold(
        calcGuiderConfig(config.guideWithAO),
        GuiderConfig(ProbeTrackingConfig.Off,
                     config.guideWithAO.map(_.toGuideSensorOption).getOrElse(GuiderSensorOff)
        )
      )

    AoTcsConfig[GuiderConfig @@ AoGuide, AltairConfig](
      gc.tcsGuide,
      TelescopeConfig(config.offsetA, config.wavelA),
      AoGuidersConfig[GuiderConfig @@ AoGuide](
        tag[P1Config](
          calcGuiderConfig(config.guideWithP1)
        ),
        tag[AoGuide](aoGuiderConfig),
        tag[OIConfig](
          calcGuiderConfig(config.guideWithOI)
        )
      ),
      AGConfig(config.lightPath, HrwfsConfig.Auto.some),
      aog,
      config.instrument
    )
  }

  def buildTcsConfig: F[TcsNorthConfig] =
    guideDb.value.map { c =>
      gaos
        .map(buildTcsAoConfig(c, _))
        .getOrElse(buildBasicTcsConfig(c))
    }

  override def nod(
    stage:  NodAndShuffleStage,
    offset: InstrumentOffset,
    guided: Boolean
  ): F[ConfigResult[F]] =
    buildTcsConfig
      .flatMap { cfg =>
        Log.debug(s"Moving to nod ${stage.symbol}") *>
          tcsController.nod(subsystems, cfg)(stage, offset, guided)
      }
      .as(ConfigResult(this))
}

object TcsNorth {

  import Tcs._

  @Lenses
  final case class TcsSeqConfig[F[_]](
    guideWithP1: Option[StandardGuideOptions.Value],
    guideWithP2: Option[StandardGuideOptions.Value],
    guideWithOI: Option[StandardGuideOptions.Value],
    guideWithAO: Option[StandardGuideOptions.Value],
    offsetA:     Option[InstrumentOffset],
    wavelA:      Option[Wavelength],
    lightPath:   LightPath,
    instrument:  InstrumentGuide
  )

  def fromConfig[F[_]: Sync: Logger](
    controller:          TcsNorthController[F],
    subsystems:          NonEmptySet[Subsystem],
    gaos:                Option[Altair[F]],
    instrument:          InstrumentGuide,
    guideConfigDb:       GuideConfigDb[F]
  )(
    config:              CleanConfig,
    lightPath:           LightPath,
    observingWavelength: Option[Wavelength]
  ): TcsNorth[F] = {

    val gwp1    = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_PWFS1_PROP).toOption
    val gwp2    = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_PWFS2_PROP).toOption
    val gwoi    = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_OIWFS_PROP).toOption
    val gwao    = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_AOWFS_PROP).toOption
    val offsetp = config
      .extractTelescopeAs[String](P_OFFSET_PROP)
      .toOption
      .flatMap(_.parseDoubleOption)
      .map(Arcseconds(_): Angle)
      .map(tag[OffsetP](_))
    val offsetq = config
      .extractTelescopeAs[String](Q_OFFSET_PROP)
      .toOption
      .flatMap(_.parseDoubleOption)
      .map(Arcseconds(_): Angle)
      .map(tag[OffsetQ](_))

    val tcsSeqCfg = TcsSeqConfig[F](
      gwp1,
      gwp2,
      gwoi,
      gwao,
      (offsetp, offsetq).mapN(InstrumentOffset(_, _)),
      observingWavelength,
      lightPath,
      instrument
    )

    new TcsNorth(controller, subsystems, gaos, guideConfigDb)(tcsSeqCfg)

  }

}
