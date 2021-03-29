// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import cats.syntax.all._
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.gemini.gems.CanopusWfs
import edu.gemini.spModel.gemini.gsaoi.GsaoiOdgw
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import org.typelevel.log4cats.Logger
import monocle.macros.Lenses
import mouse.all._
import seqexec.model.enum.M1Source
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.Resource
import seqexec.model.enum.TipTiltSource
import seqexec.server.CleanConfig
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigResult
import seqexec.server.ConfigUtilOps._
import seqexec.server.InstrumentGuide
import seqexec.server.SeqexecFailure
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.tcs.TcsController.AGConfig
import seqexec.server.tcs.TcsController.AoGuidersConfig
import seqexec.server.tcs.TcsController.AoTcsConfig
import seqexec.server.tcs.TcsController.BasicGuidersConfig
import seqexec.server.tcs.TcsController.BasicTcsConfig
import seqexec.server.tcs.TcsController.GuiderConfig
import seqexec.server.tcs.TcsController.GuiderSensorOff
import seqexec.server.tcs.TcsController.HrwfsConfig
import seqexec.server.tcs.TcsController.InstrumentOffset
import seqexec.server.tcs.TcsController.LightPath
import seqexec.server.tcs.TcsController.OIConfig
import seqexec.server.tcs.TcsController.OffsetP
import seqexec.server.tcs.TcsController.OffsetQ
import seqexec.server.tcs.TcsController.P1Config
import seqexec.server.tcs.TcsController.P2Config
import seqexec.server.tcs.TcsController.ProbeTrackingConfig
import seqexec.server.tcs.TcsController.Subsystem
import seqexec.server.tcs.TcsController.TelescopeConfig
import seqexec.server.tcs.TcsSouthController.CWFS1Config
import seqexec.server.tcs.TcsSouthController.CWFS2Config
import seqexec.server.tcs.TcsSouthController.CWFS3Config
import seqexec.server.tcs.TcsSouthController.GemsGuiders
import seqexec.server.tcs.TcsSouthController.ODGW1Config
import seqexec.server.tcs.TcsSouthController.ODGW2Config
import seqexec.server.tcs.TcsSouthController.ODGW3Config
import seqexec.server.tcs.TcsSouthController.ODGW4Config
import seqexec.server.tcs.TcsSouthController.TcsSouthConfig
import shapeless.tag
import squants.Angle
import squants.space.Arcseconds

case class TcsSouth[F[_]: Sync: Logger] private (
  tcsController: TcsSouthController[F],
  subsystems:    NonEmptySet[Subsystem],
  gaos:          Option[Gems[F]],
  guideDb:       GuideConfigDb[F]
)(config:        TcsSouth.TcsSeqConfig[F])
    extends Tcs[F] {
  import Tcs.{ GuideWithOps, calcGuiderInUse }

  val Log: Logger[F] = Logger[F]

  override val resource: Resource = Resource.TCS

  //TODO Implement GeMS case
  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsSouthConfig, subsystem: Subsystem): String =
    (subsystem match {
      case Subsystem.M1     => pprint.apply(tcs.gc.m1Guide)
      case Subsystem.M2     => pprint.apply(tcs.gc.m2Guide)
      case Subsystem.OIWFS  => pprint.apply(tcs.gds.oiwfs)
      case Subsystem.PWFS1  => pprint.apply(tcs.gds.pwfs1)
      case Subsystem.PWFS2  => pprint.apply(tcs.gds.pwfs2)
      case Subsystem.Mount  => pprint.apply(tcs.tc)
      case Subsystem.AGUnit => pprint.apply(List(tcs.agc.sfPos, tcs.agc.hrwfs))
      case Subsystem.Gaos   => pprint.apply("")
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

  val defaultGuiderConf = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
  def calcGuiderConfig(
    inUse:     Boolean,
    guideWith: Option[StandardGuideOptions.Value]
  ): GuiderConfig =
    guideWith
      .flatMap(v => inUse.option(GuiderConfig(v.toProbeTracking, v.toGuideSensorOption)))
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
        tag[P1Config](
          calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.PWFS1, M1Source.PWFS1),
                           config.guideWithP1
          )
        ),
        tag[P2Config](
          calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.PWFS2, M1Source.PWFS2),
                           config.guideWithP2
          )
        ),
        tag[OIConfig](
          calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.OIWFS, M1Source.OIWFS),
                           config.guideWithOI
          )
        )
      ),
      AGConfig(config.lightPath, HrwfsConfig.Auto.some),
      config.instrument
    ): TcsSouthConfig).pure[F]

  private def buildTcsAoConfig(gc: GuideConfig): F[TcsSouthConfig] =
    gc.gaosGuide
      .flatMap(_.toOption)
      .map { aog =>
        AoTcsConfig[GemsGuiders, GemsConfig](
          gc.tcsGuide,
          TelescopeConfig(config.offsetA, config.wavelA),
          AoGuidersConfig[GemsGuiders](
            tag[P1Config](
              calcGuiderConfig(
                calcGuiderInUse(gc.tcsGuide, TipTiltSource.PWFS1, M1Source.PWFS1) | aog.isP1Used,
                config.guideWithP1
              )
            ),
            GemsGuiders(
              tag[CWFS1Config](
                calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                                 config.guideWithCWFS1
                )
              ),
              tag[CWFS2Config](
                calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                                 config.guideWithCWFS2
                )
              ),
              tag[CWFS3Config](
                calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                                 config.guideWithCWFS3
                )
              ),
              tag[ODGW1Config](
                calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                                 config.guideWithODGW1
                )
              ),
              tag[ODGW2Config](
                calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                                 config.guideWithODGW2
                )
              ),
              tag[ODGW3Config](
                calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                                 config.guideWithODGW3
                )
              ),
              tag[ODGW4Config](
                calcGuiderConfig(calcGuiderInUse(gc.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS),
                                 config.guideWithODGW4
                )
              )
            ),
            tag[OIConfig](
              calcGuiderConfig(
                calcGuiderInUse(gc.tcsGuide, TipTiltSource.OIWFS, M1Source.OIWFS) | aog.isOIUsed,
                config.guideWithOI
              )
            )
          ),
          AGConfig(config.lightPath, HrwfsConfig.Auto.some),
          aog,
          config.instrument
        ): TcsSouthConfig
      }
      .map(_.pure[F])
      .getOrElse(
        SeqexecFailure
          .Execution("Attempting to run GeMS sequence before GeMS was configured.")
          .raiseError[F, TcsSouthConfig]
      )

  def buildTcsConfig: F[TcsSouthConfig] =
    guideDb.value.flatMap { c =>
      if (gaos.isDefined) buildTcsAoConfig(c)
      else buildBasicTcsConfig(c)
    }

}

object TcsSouth {

  import Tcs._

  @Lenses
  final case class TcsSeqConfig[F[_]](
    guideWithP1:    Option[StandardGuideOptions.Value],
    guideWithP2:    Option[StandardGuideOptions.Value],
    guideWithOI:    Option[StandardGuideOptions.Value],
    guideWithCWFS1: Option[StandardGuideOptions.Value],
    guideWithCWFS2: Option[StandardGuideOptions.Value],
    guideWithCWFS3: Option[StandardGuideOptions.Value],
    guideWithODGW1: Option[StandardGuideOptions.Value],
    guideWithODGW2: Option[StandardGuideOptions.Value],
    guideWithODGW3: Option[StandardGuideOptions.Value],
    guideWithODGW4: Option[StandardGuideOptions.Value],
    offsetA:        Option[InstrumentOffset],
    wavelA:         Option[Wavelength],
    lightPath:      LightPath,
    instrument:     InstrumentGuide
  )

  def fromConfig[F[_]: Sync: Logger](
    controller:          TcsSouthController[F],
    subsystems:          NonEmptySet[Subsystem],
    gaos:                Option[Gems[F]],
    instrument:          InstrumentGuide,
    guideConfigDb:       GuideConfigDb[F]
  )(
    config:              CleanConfig,
    lightPath:           LightPath,
    observingWavelength: Option[Wavelength]
  ): TcsSouth[F] = {

    val gwp1    = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_PWFS1_PROP).toOption
    val gwp2    = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_PWFS2_PROP).toOption
    val gwoi    = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_OIWFS_PROP).toOption
    val gwc1    = config
      .extractTelescopeAs[StandardGuideOptions.Value](CanopusWfs.cwfs1.getSequenceProp)
      .toOption
    val gwc2    = config
      .extractTelescopeAs[StandardGuideOptions.Value](CanopusWfs.cwfs2.getSequenceProp)
      .toOption
    val gwc3    = config
      .extractTelescopeAs[StandardGuideOptions.Value](CanopusWfs.cwfs3.getSequenceProp)
      .toOption
    val gwod1   = config
      .extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw1.getSequenceProp)
      .toOption
    val gwod2   = config
      .extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw2.getSequenceProp)
      .toOption
    val gwod3   = config
      .extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw3.getSequenceProp)
      .toOption
    val gwod4   = config
      .extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw4.getSequenceProp)
      .toOption
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
      gwc1,
      gwc2,
      gwc3,
      gwod1,
      gwod2,
      gwod3,
      gwod4,
      (offsetp, offsetq).mapN(InstrumentOffset(_, _)),
      observingWavelength,
      lightPath,
      instrument
    )

    new TcsSouth(controller, subsystems, gaos, guideConfigDb)(tcsSeqCfg)

  }

}
