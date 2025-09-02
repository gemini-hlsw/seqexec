// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.Resource
import seqexec.server.CleanConfig
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigResult
import seqexec.server.ConfigUtilOps._
import seqexec.server.InstrumentGuide
import seqexec.server.gems.Gems
import seqexec.server.gems.GemsController.{ GemsConfig, GemsOff }
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
import squants.space.Length

case class TcsSouth[F[_]: Sync: Logger] private (
  tcsController: TcsSouthController[F],
  subsystems:    NonEmptySet[Subsystem],
  gaos:          Option[Gems[F]],
  guideDb:       GuideConfigDb[F]
)(config: TcsSouth.TcsSeqConfig[F])
    extends Tcs[F] {

  import Tcs.GuideWithOps

  val Log: Logger[F] = Logger[F]

  override val resource: Resource = Resource.TCS

  override def configure(config: CleanConfig): F[ConfigResult[F]] =
    buildTcsConfig.flatMap { cfg =>
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

  private val defaultGuiderConf = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)

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
  def buildBasicTcsConfig(gc: GuideConfig): TcsSouthConfig =
    BasicTcsConfig(
      gc.tcsGuide,
      TelescopeConfig(config.offsetA, config.wavelA, config.instrumentDefocus),
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

  private def buildTcsAoConfig(gc: GuideConfig): TcsSouthConfig = {
    val aog = gc.gaosGuide.flatMap(_.toOption).getOrElse(GemsOff)

    AoTcsConfig[GemsGuiders, GemsConfig](
      gc.tcsGuide,
      TelescopeConfig(config.offsetA, config.wavelA, config.instrumentDefocus),
      AoGuidersConfig[GemsGuiders](
        tag[P1Config](
          calcGuiderConfig(config.guideWithP1)
        ),
        GemsGuiders(
          tag[CWFS1Config](
            calcGuiderConfig(config.guideWithCWFS1)
          ),
          tag[CWFS2Config](
            calcGuiderConfig(config.guideWithCWFS2)
          ),
          tag[CWFS3Config](
            calcGuiderConfig(config.guideWithCWFS3)
          ),
          tag[ODGW1Config](
            calcGuiderConfig(config.guideWithODGW1)
          ),
          tag[ODGW2Config](
            calcGuiderConfig(config.guideWithODGW2)
          ),
          tag[ODGW3Config](
            calcGuiderConfig(config.guideWithODGW3)
          ),
          tag[ODGW4Config](
            calcGuiderConfig(config.guideWithODGW4)
          )
        ),
        tag[OIConfig](
          calcGuiderConfig(config.guideWithOI)
        )
      ),
      AGConfig(config.lightPath, HrwfsConfig.Auto.some),
      aog,
      config.instrument
    )
  }

  def buildTcsConfig: F[TcsSouthConfig] =
    guideDb.value.map { c =>
      if (gaos.isDefined) buildTcsAoConfig(c)
      else buildBasicTcsConfig(c)
    }

}

object TcsSouth {

  import Tcs._

  @Lenses
  final case class TcsSeqConfig[F[_]](
    guideWithP1:       Option[StandardGuideOptions.Value],
    guideWithP2:       Option[StandardGuideOptions.Value],
    guideWithOI:       Option[StandardGuideOptions.Value],
    guideWithCWFS1:    Option[StandardGuideOptions.Value],
    guideWithCWFS2:    Option[StandardGuideOptions.Value],
    guideWithCWFS3:    Option[StandardGuideOptions.Value],
    guideWithODGW1:    Option[StandardGuideOptions.Value],
    guideWithODGW2:    Option[StandardGuideOptions.Value],
    guideWithODGW3:    Option[StandardGuideOptions.Value],
    guideWithODGW4:    Option[StandardGuideOptions.Value],
    offsetA:           Option[InstrumentOffset],
    wavelA:            Option[Wavelength],
    instrumentDefocus: Option[Length],
    lightPath:         LightPath,
    instrument:        InstrumentGuide
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
    observingWavelength: Option[Wavelength],
    instrumentDefocus:   Option[
      Length
    ] // We may add this to the sequence, in that case this param could be removed
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
      instrumentDefocus,
      lightPath,
      instrument
    )

    new TcsSouth(controller, subsystems, gaos, guideConfigDb)(tcsSeqCfg)

  }

}
