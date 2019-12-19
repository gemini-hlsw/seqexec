// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.MonadError
import cats.data.NonEmptySet
import cats.effect.Sync
import cats.implicits._
import mouse.all._
import seqexec.model.enum.{M1Source, NodAndShuffleStage, Resource, TipTiltSource}
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.guide.StandardGuideOptions
import io.chrisdavenport.log4cats.Logger
import monocle.macros.Lenses
import seqexec.server.{CleanConfig, ConfigResult, InstrumentSystem, SeqexecFailure}
import seqexec.server.CleanConfig.extractItem
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.tcs.TcsController._
import seqexec.server.tcs.TcsNorthController.{TcsNorthAoConfig, TcsNorthConfig}
import seqexec.server.ConfigUtilOps._
import shapeless.tag
import shapeless.tag.@@
import squants.Angle
import squants.space.Arcseconds

class TcsNorth[F[_]: Sync: MonadError[?[_], Throwable]: Logger] private(tcsController: TcsNorthController[F],
                                                                        subsystems: NonEmptySet[Subsystem],
                                                                        gaos: Option[Altair[F]],
                                                                        guideDb: GuideConfigDb[F]
                                   )(config: TcsNorth.TcsSeqConfig[F]) extends Tcs[F] {
  import Tcs.{GuideWithOps, calcGuiderInUse}

  val Log: Logger[F] = Logger[F]

  override val resource: Resource = Resource.TCS

  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsNorthConfig, subsystem: Subsystem): List[String] = subsystem match {
    case Subsystem.M1     => List(tcs.gc.m1Guide.show)
    case Subsystem.M2     => List(tcs.gc.m2Guide.show)
    case Subsystem.OIWFS  => List((tcs.gds.oiwfs:GuiderConfig).show)
    case Subsystem.PWFS1  => List((tcs.gds.pwfs1:GuiderConfig).show)
    case Subsystem.PWFS2  => List((tcs.gds.pwfs2:GuiderConfig).show)
    case Subsystem.Mount  => List(tcs.tc.show)
    case Subsystem.AGUnit => List(tcs.agc.sfPos.show, tcs.agc.hrwfs.show)
    case Subsystem.Gaos   => tcs match {
      case x:TcsNorthAoConfig => List((x.gds.aoguide:GuiderConfig).show)
      case _                  => List.empty
    }
  }

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    buildTcsConfig.flatMap{ cfg =>
      Log.debug(s"Applying TCS configuration: ${subsystems.toList.flatMap(subsystemConfig(cfg, _))}") *>
        tcsController.applyConfig(subsystems, gaos, cfg).as(ConfigResult(this))
    }

  override def notifyObserveStart: F[Unit] = tcsController.notifyObserveStart

  override def notifyObserveEnd: F[Unit] = tcsController.notifyObserveEnd

  val defaultGuiderConf = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
  def calcGuiderConfig(inUse: Boolean, guideWith: Option[StandardGuideOptions.Value]): GuiderConfig =
    guideWith.flatMap(v => inUse.option(GuiderConfig(v.toProbeTracking, v.toGuideSensorOption)))
      .getOrElse(defaultGuiderConf)

  /*
   * Build TCS configuration for the step, merging the guide configuration from the sequence with the guide
   * configuration set from TCC. The TCC configuration has precedence: if a guider is not used in the TCC configuration,
   * it will not be used for the step, regardless of the sequence values.
   */
  private def buildBasicTcsConfig(gc: GuideConfig): F[TcsNorthConfig] =
    (BasicTcsConfig(
      gc.tcsGuide,
      TelescopeConfig(config.offsetA, config.wavelA),
      BasicGuidersConfig(
        tag[P1Config](calcGuiderConfig(
          calcGuiderInUse(gc.tcsGuide, TipTiltSource.PWFS1, M1Source.PWFS1),
          config.guideWithP1)
        ),
        tag[P2Config](calcGuiderConfig(
          calcGuiderInUse(gc.tcsGuide, TipTiltSource.PWFS2, M1Source.PWFS2),
          config.guideWithP2)
        ),
        tag[OIConfig](calcGuiderConfig(
          calcGuiderInUse(gc.tcsGuide, TipTiltSource.OIWFS, M1Source.OIWFS),
          config.guideWithOI)
        )
      ),
      AGConfig(config.lightPath, HrwfsConfig.Auto.some),
      config.instrument
    ):TcsNorthConfig).pure[F]

  private def buildTcsAoConfig(gc: GuideConfig, ao: Altair[F]): F[TcsNorthConfig] = {
    gc.gaosGuide.flatMap(_.swap.toOption.map{ aog =>

      val aoGuiderConfig = ao.hasTarget(aog).fold(
        calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS), config.guideWithAO),
        GuiderConfig(ProbeTrackingConfig.Off, config.guideWithAO.map(_.toGuideSensorOption).getOrElse(GuiderSensorOff))
      )

      AoTcsConfig[GuiderConfig@@AoGuide, AltairConfig](
        gc.tcsGuide,
        TelescopeConfig(config.offsetA, config.wavelA),
        AoGuidersConfig[GuiderConfig@@AoGuide](
          tag[P1Config](calcGuiderConfig(
            calcGuiderInUse(gc.tcsGuide, TipTiltSource.PWFS1, M1Source.PWFS1) | ao.usesP1(aog),
            config.guideWithP1)
          ),
          tag[AoGuide](aoGuiderConfig),
          tag[OIConfig](calcGuiderConfig(
            calcGuiderInUse(gc.tcsGuide, TipTiltSource.OIWFS, M1Source.OIWFS) | ao.usesOI(aog),
            config.guideWithOI)
          )
        ),
        AGConfig(config.lightPath, HrwfsConfig.Auto.some),
        aog,
        config.instrument
      ):TcsNorthConfig
    }).map(_.pure[F])
      .getOrElse(SeqexecFailure.Execution("Attempting to run Altair sequence before Altair has being configured.")
        .raiseError[F, TcsNorthConfig])
  }


  def buildTcsConfig: F[TcsNorthConfig] =
    guideDb.value.flatMap{ c =>
      gaos.map(buildTcsAoConfig(c, _))
        .getOrElse(buildBasicTcsConfig(c)
      )
    }

  override def nod(stage: NodAndShuffleStage, offset: InstrumentOffset, guided: Boolean): F[ConfigResult[F]] =
    buildTcsConfig.flatMap{ cfg =>
      Log.debug(s"Moving to nod ${stage.symbol}") *>
        tcsController.nod(subsystems, cfg)(stage, offset, guided)
    }.as(ConfigResult(this))
}

object TcsNorth {

  import Tcs._

  @Lenses
  final case class TcsSeqConfig[F[_]](
                                       guideWithP1: Option[StandardGuideOptions.Value],
                                       guideWithP2: Option[StandardGuideOptions.Value],
                                       guideWithOI: Option[StandardGuideOptions.Value],
                                       guideWithAO: Option[StandardGuideOptions.Value],
                                       offsetA: Option[InstrumentOffset],
                                       wavelA: Option[Wavelength],
                                       lightPath: LightPath,
                                       instrument: InstrumentSystem[F]
                                     )

  def fromConfig[F[_]: Sync: Logger](controller: TcsNorthController[F],
                             subsystems: NonEmptySet[Subsystem],
                             gaos: Option[Altair[F]],
                             instrument: InstrumentSystem[F],
                             guideConfigDb: GuideConfigDb[F]
                            )(config: CleanConfig,
                              lightPath: LightPath,
                              observingWavelength: Option[Wavelength]
  ): TcsNorth[F] = {

    val gwp1 = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_PWFS1_PROP).toOption
    val gwp2 = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_PWFS2_PROP).toOption
    val gwoi = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_OIWFS_PROP).toOption
    val gwao = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_AOWFS_PROP).toOption
    val offsetp = config.extractTelescopeAs[String](P_OFFSET_PROP).toOption.flatMap(_.parseDoubleOption)
      .map(Arcseconds(_):Angle).map(tag[OffsetP](_))
    val offsetq = config.extractTelescopeAs[String](Q_OFFSET_PROP).toOption.flatMap(_.parseDoubleOption)
      .map(Arcseconds(_):Angle).map(tag[OffsetQ](_))

    val tcsSeqCfg = TcsSeqConfig(
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
