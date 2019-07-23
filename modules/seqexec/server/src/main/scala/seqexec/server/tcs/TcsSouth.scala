// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import cats.implicits._
import mouse.all._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.seqcomp.SeqConfigNames.TELESCOPE_KEY
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import monocle.macros.Lenses
import org.log4s.getLogger
import seqexec.model.enum.{M1Source, Resource, TipTiltSource}
import seqexec.server.{ConfigResult, InstrumentSystem, System}
import seqexec.server.gems.Gems
import seqexec.server.tcs.TcsController.{AGConfig, BasicGuidersConfig, BasicTcsConfig, GuiderConfig, GuiderSensorOff, HrwfsConfig, InstrumentOffset, LightPath, OIConfig, OffsetP, OffsetQ, P1Config, P2Config, ProbeTrackingConfig, Subsystem, TelescopeConfig}
import seqexec.server.ConfigUtilOps._
import seqexec.server.tcs.TcsSouthController.TcsSouthConfig
import shapeless.tag
import squants.Angle
import squants.space.Arcseconds

case class TcsSouth [F[_]: Sync] private (tcsController: TcsSouthController[F],
                                     subsystems: NonEmptySet[Subsystem],
                                     gaos: Option[Gems[F]],
                                     guideDb: GuideConfigDb[F]
                                    )(config: TcsSouth.TcsSeqConfig[F]) extends System[F] {
  import TcsSouth._
  import Tcs.{GuideWithOps, calcGuiderInUse}

  override val resource: Resource = Resource.TCS

  //TODO Implement GeMS case
  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsSouthConfig, subsystem: Subsystem): List[String] = subsystem match {
    case Subsystem.M1     => List(tcs.gc.m1Guide.show)
    case Subsystem.M2     => List(tcs.gc.m2Guide.show)
    case Subsystem.OIWFS  => List((tcs.gds.oiwfs:GuiderConfig).show)
    case Subsystem.PWFS1  => List((tcs.gds.pwfs1:GuiderConfig).show)
    case Subsystem.PWFS2  => List((tcs.gds.pwfs2:GuiderConfig).show)
    case Subsystem.Mount  => List(tcs.tc.show)
    case Subsystem.AGUnit => List(tcs.agc.sfPos.show, tcs.agc.hrwfs.show)
    case Subsystem.Gaos   => List()
  }

  override def configure(config: Config): F[ConfigResult[F]] =
    buildTcsConfig.flatMap{ cfg =>
      Log.debug(s"Applying TCS configuration: ${subsystems.toList.flatMap(subsystemConfig(cfg, _))}").pure[F] *>
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
  def buildBasicTcsConfig(gc: GuideConfig): F[TcsSouthConfig] =
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
    ):TcsSouthConfig).pure[F]

  def buildTcsConfig: F[TcsSouthConfig] = guideDb.value.flatMap(buildBasicTcsConfig)

}

object TcsSouth {

  import Tcs._

  private val Log = getLogger

  @Lenses
  final case class TcsSeqConfig[F[_]](
                                       guideWithP1: Option[StandardGuideOptions.Value],
                                       guideWithP2: Option[StandardGuideOptions.Value],
                                       guideWithOI: Option[StandardGuideOptions.Value],
                                       offsetA: Option[InstrumentOffset],
                                       wavelA: Option[Wavelength],
                                       lightPath: LightPath,
                                       instrument: InstrumentSystem[F]
                                     )

  def fromConfig[F[_]: Sync](controller: TcsSouthController[F], subsystems: NonEmptySet[Subsystem],
                             gaos: Option[Gems[F]], instrument: InstrumentSystem[F], guideConfigDb: GuideConfigDb[F])(
    config: Config, lightPath: LightPath, observingWavelength: Option[Wavelength]
  ): TcsSouth[F] = {

    val gwp1 = config.extractAs[StandardGuideOptions.Value](TELESCOPE_KEY / GUIDE_WITH_PWFS1_PROP).toOption
    val gwp2 = config.extractAs[StandardGuideOptions.Value](TELESCOPE_KEY / GUIDE_WITH_PWFS2_PROP).toOption
    val gwoi = config.extractAs[StandardGuideOptions.Value](TELESCOPE_KEY / GUIDE_WITH_OIWFS_PROP).toOption
    val offsetp = config.extractAs[String](TELESCOPE_KEY / P_OFFSET_PROP).toOption.flatMap(_.parseDoubleOption)
      .map(Arcseconds(_):Angle).map(tag[OffsetP](_))
    val offsetq = config.extractAs[String](TELESCOPE_KEY / Q_OFFSET_PROP).toOption.flatMap(_.parseDoubleOption)
      .map(Arcseconds(_):Angle).map(tag[OffsetQ](_))

    val tcsSeqCfg = TcsSeqConfig(
      gwp1,
      gwp2,
      gwoi,
      (offsetp, offsetq).mapN(InstrumentOffset(_, _)),
      observingWavelength,
      lightPath,
      instrument
    )

    new TcsSouth(controller, subsystems, gaos, guideConfigDb)(tcsSeqCfg)

  }

}
