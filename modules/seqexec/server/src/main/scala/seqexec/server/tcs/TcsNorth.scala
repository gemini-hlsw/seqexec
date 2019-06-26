// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.NonEmptySet
import cats.effect.Sync
import cats.implicits._
import mouse.all._
import seqexec.model.enum.TipTiltSource
import seqexec.model.enum.M1Source
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.seqcomp.SeqConfigNames.TELESCOPE_KEY
import monocle.macros.Lenses
import org.log4s.getLogger
import seqexec.model.enum.Resource
import seqexec.server.{ConfigResult, InstrumentSystem, SeqActionF, System}
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController.AltairOff
import seqexec.server.tcs.TcsController.{AGConfig, AoGuide, GuiderConfig, GuiderSensorOff, HrwfsConfig, InstrumentOffset, LightPath, OIConfig, OffsetP, OffsetQ, P1Config, P2Config, ProbeTrackingConfig, Subsystem, TelescopeConfig}
import seqexec.server.tcs.TcsNorthController.{GuidersConfig, TcsNorthConfig}
import seqexec.server.ConfigUtilOps._
import shapeless.tag
import squants.Angle
import squants.space.Arcseconds

class TcsNorth[F[_]: Sync] private (tcsController: TcsNorthController[F],
                                    subsystems: NonEmptySet[Subsystem],
                                    gaos: Option[Altair[F]],
                                    guideDb: GuideConfigDb[F]
                                   )(config: TcsNorth.TcsSeqConfig[F]) extends System[F] {
  import TcsNorth._
  import Tcs.{GuideWithOps, calcGuiderInUse}

  override val resource: Resource = Resource.TCS

  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsNorthConfig, subsystem: Subsystem): List[String] = subsystem match {
    case Subsystem.M1     => List(tcs.gc.m1Guide.show)
    case Subsystem.M2     => List(tcs.gc.m2Guide.show)
    case Subsystem.OIWFS  => List((tcs.gds.oiwfs:GuiderConfig).show)
    case Subsystem.PWFS1  => List((tcs.gds.pwfs1:GuiderConfig).show)
    case Subsystem.PWFS2  => List(tcs.gds.pwfs2OrAowfs.swap.getOrElse(
      GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)).show)
    case Subsystem.Mount  => List(tcs.tc.show)
    case Subsystem.AGUnit => List(tcs.agc.sfPos.show, tcs.agc.hrwfs.show)
    case Subsystem.Gaos   => tcs.gds.pwfs2OrAowfs.map((_:GuiderConfig).show).toList
  }

  override def configure(config: Config): SeqActionF[F, ConfigResult[F]] = SeqActionF.embedF(
    buildTcsConfig.flatMap{ cfg =>
      Log.debug(s"Applying TCS configuration: ${subsystems.toList.flatMap(subsystemConfig(cfg, _))}").pure[F] *>
        tcsController.applyConfig(subsystems, gaos, cfg).as(ConfigResult(this))
    }
  )

  override def notifyObserveStart: SeqActionF[F, Unit] = SeqActionF.embedF(tcsController.notifyObserveStart)

  override def notifyObserveEnd: SeqActionF[F, Unit] = SeqActionF.embedF(tcsController.notifyObserveEnd)

  val defaultGuiderConf = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
  def calcGuiderConfig(inUse: Boolean, guideWith: Option[StandardGuideOptions.Value]): GuiderConfig =
    guideWith.flatMap(v => inUse.option(GuiderConfig(v.toProbeTracking, v.toGuideSensorOption)))
      .getOrElse(defaultGuiderConf)

  private val usesAltair = gaos.isDefined
  /*
   * Build TCS configuration for the step, merging the guide configuration from the sequence with the guide
   * configuration set from TCC. The TCC configuration has precedence: if a guider is not used in the TCC configuration,
   * it will not be used for the step, regardless of the sequence values.
   */
  def buildTcsConfig: F[TcsNorthConfig] =
    guideDb.value.map{ c => {
      val useAo: Boolean = c.gaosGuide match {
        case Some(Left(AltairOff)) => false
        case Some(Left(_))         => true
        case _                     => false
      }
      val aoUsesP1 = useAo && (gaos, c.gaosGuide.flatMap(_.swap.toOption)).mapN(_.usesP1(_))
        .getOrElse(false)
      val aoUsesOI = useAo && (gaos, c.gaosGuide.flatMap(_.swap.toOption)).mapN(_.usesOI(_))
        .getOrElse(false)

      val aoHasTarget = useAo && (gaos, c.gaosGuide.flatMap(_.swap.toOption)).mapN(_
        .hasTarget(_)).getOrElse(false)

      val aoGuiderConfig = aoHasTarget.fold(
        calcGuiderConfig(calcGuiderInUse(c.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS), config.guideWithAO),
        GuiderConfig(ProbeTrackingConfig.Off, config.guideWithAO.map(_.toGuideSensorOption).getOrElse(GuiderSensorOff))
      )

      TcsNorthConfig(
        c.tcsGuide,
        TelescopeConfig(config.offsetA, config.wavelA),
        GuidersConfig(
          tag[P1Config](calcGuiderConfig(
            calcGuiderInUse(c.tcsGuide, TipTiltSource.PWFS1, M1Source.PWFS1) | aoUsesP1,
            config.guideWithP1)
          ),
          usesAltair.either(
            tag[P2Config](calcGuiderConfig(calcGuiderInUse(c.tcsGuide, TipTiltSource.PWFS2, M1Source.PWFS2),
              config.guideWithP2)),
            tag[AoGuide](aoGuiderConfig)
          ),
          tag[OIConfig](calcGuiderConfig(
            calcGuiderInUse(c.tcsGuide, TipTiltSource.OIWFS, M1Source.OIWFS) | aoUsesOI,
            config.guideWithOI)
          )
        ),
        AGConfig(config.lightPath, HrwfsConfig.Auto.some),
        c.gaosGuide.flatMap(_.swap.toOption),
        config.instrument
      )
    }
  }

}

object TcsNorth {

  import Tcs._

  private val Log = getLogger

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

  def fromConfig[F[_]: Sync](controller: TcsNorthController[F], subsystems: NonEmptySet[Subsystem],
                             gaos: Option[Altair[F]], instrument: InstrumentSystem[F], guideConfigDb: GuideConfigDb[F])(
    config: Config, lightPath: LightPath, observingWavelength: Option[Wavelength]
  ): TcsNorth[F] = {

    val gwp1 = config.extractAs[StandardGuideOptions.Value](TELESCOPE_KEY / GUIDE_WITH_PWFS1_PROP).toOption
    val gwp2 = config.extractAs[StandardGuideOptions.Value](TELESCOPE_KEY / GUIDE_WITH_PWFS2_PROP).toOption
    val gwoi = config.extractAs[StandardGuideOptions.Value](TELESCOPE_KEY / GUIDE_WITH_OIWFS_PROP).toOption
    val gwao = config.extractAs[StandardGuideOptions.Value](TELESCOPE_KEY / GUIDE_WITH_AOWFS_PROP).toOption
    val offsetp = config.extractAs[String](TELESCOPE_KEY / P_OFFSET_PROP).toOption.flatMap(_.parseDoubleOption)
      .map(Arcseconds(_):Angle).map(tag[OffsetP](_))
    val offsetq = config.extractAs[String](TELESCOPE_KEY / Q_OFFSET_PROP).toOption.flatMap(_.parseDoubleOption)
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