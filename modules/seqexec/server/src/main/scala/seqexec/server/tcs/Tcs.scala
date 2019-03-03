// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.data.{EitherT, NonEmptySet}
import cats.effect.IO
import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.core.Wavelength
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.seqcomp.SeqConfigNames.TELESCOPE_KEY
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants._
import monocle.macros.Lenses
import org.log4s.getLogger
import mouse.all._
import seqexec.model.enum.Resource
import seqexec.server.ConfigUtilOps._
import seqexec.server.altair.Altair
import seqexec.server.altair.AltairController._
import seqexec.server.gems.Gems
import seqexec.server.tcs.TcsController._
import seqexec.server.{ConfigResult, SeqAction, SeqexecFailure, System}
import shapeless.tag
import squants.Angle
import squants.space.Arcseconds


final case class Tcs private (tcsController: TcsController,
                              subsystems: NonEmptySet[Subsystem],
                              gaos: Option[Either[Altair[IO], Gems[IO]]],
                              guideDb: GuideConfigDb[IO]
                             )(config: Tcs.TcsSeqConfig) extends System[IO] {
  import Tcs._

  override val resource: Resource = Resource.TCS

  // Helper function to output the part of the TCS configuration that is actually applied.
  private def subsystemConfig(tcs: TcsConfig, subsystem: Subsystem): List[String] = subsystem match {
    case Subsystem.M1     => List(tcs.gc.m1Guide.show)
    case Subsystem.M2     => List(tcs.gc.m2Guide.show)
    case Subsystem.OIWFS  => List((tcs.gds.oiwfs:GuiderConfig).show)
    case Subsystem.PWFS1  => List((tcs.gds.pwfs1:GuiderConfig).show)
    case Subsystem.PWFS2  => List((tcs.gds.pwfs2:GuiderConfig).show)
    case Subsystem.Mount  => List(tcs.tc.show)
    case Subsystem.AGUnit => List(tcs.agc.sfPos.show, tcs.agc.hrwfs.show)
    case Subsystem.Gaos   => List("") //TODO: show Gaos configuration
  }

  override def configure(config: Config): SeqAction[ConfigResult[IO]] =
    EitherT.liftF[IO, SeqexecFailure, TcsConfig](buildTcsConfig)
      .flatMap{ cfg =>
        SeqAction(Log.debug(s"Applying TCS configuration: ${subsystems.toList.flatMap(subsystemConfig(cfg, _))}")) *>
        tcsController.applyConfig(subsystems, gaos, cfg).as(ConfigResult(this))
      }

  override def notifyObserveStart: SeqAction[Unit] =
    tcsController.notifyObserveStart

  override def notifyObserveEnd: SeqAction[Unit] = tcsController.notifyObserveEnd

  def calcGuiderInUse(telGuide: TelescopeGuideConfig, tipTiltSource: TipTiltSource, m1Source: M1Source): Boolean = {
    val usedByM1: Boolean = telGuide.m1Guide match {
      case M1GuideOn(src) => src === m1Source
      case _              => false
    }
    val usedByM2 = telGuide.m2Guide match {
      case M2GuideOn(_, srcs) => srcs.contains(tipTiltSource)
      case _                  => false
    }

    usedByM1 | usedByM2
  }

  val defaultGuiderConf = GuiderConfig(ProbeTrackingConfig.Parked, GuiderSensorOff)
  def calcGuiderConfig(inUse: Boolean, guideWith: Option[StandardGuideOptions.Value]): GuiderConfig =
    guideWith.flatMap(v => inUse.option(GuiderConfig(v.toProbeTracking, v.toGuideSensorOption)))
      .getOrElse(defaultGuiderConf)

  /*
   * Build TCS configuration for the step, merging the guide configuration from the sequence with the guide
   * configuration set from TCC. The TCC configuration has precedence: if a guider is not used in the TCC configuration,
   * it will not be used for the step, regardless of the sequence values.
   */
  def buildTcsConfig: IO[TcsConfig] =
    guideDb.value.map{ c => {
      val useAo: Boolean = c.gaosGuide match {
        case Some(Left(AltairOff)) => false
        case Some(Left(_))         => true
        case _                     => false
      }
      val aoUsesP1 = (gaos.flatMap(_.swap.toOption), c.gaosGuide.flatMap(_.swap.toOption)).mapN(_.usesP1(_))
        .getOrElse(false)
      val aoUsesOI = (gaos.flatMap(_.swap.toOption), c.gaosGuide.flatMap(_.swap.toOption)).mapN(_.usesOI(_))
        .getOrElse(false)

      TcsConfig(
        c.tcsGuide,
        TelescopeConfig(config.offsetA, config.wavelA),
        GuidersConfig(
          tag[P1Config](calcGuiderConfig(
            calcGuiderInUse(c.tcsGuide, TipTiltSource.PWFS1, M1Source.PWFS1) | aoUsesP1,
            config.guideWithP1)
          ),
          tag[P2Config](calcGuiderConfig(
            calcGuiderInUse(c.tcsGuide, TipTiltSource.PWFS2, M1Source.PWFS2),
            config.guideWithP2)
          ),
          tag[OIConfig](calcGuiderConfig(
            calcGuiderInUse(c.tcsGuide, TipTiltSource.OIWFS, M1Source.OIWFS) | aoUsesOI,
            config.guideWithOI)
          ),
          tag[AOGuide](useAo & calcGuiderInUse(c.tcsGuide, TipTiltSource.GAOS, M1Source.GAOS) &
            config.guideWithAO.exists(_.isActive))
        ),
        AGConfig(config.scienceFoldPosition, HrwfsConfig.Auto.some),
        c.gaosGuide
      )
    }
  }
}

object Tcs {
  private val Log = getLogger

  // Shouldn't these be defined somewhere ?
  val GUIDE_WITH_PWFS1_PROP: String = "guideWithPWFS1"
  val GUIDE_WITH_PWFS2_PROP: String = "guideWithPWFS2"
  val GUIDE_WITH_AOWFS_PROP: String = "guideWithAOWFS"
  val P_OFFSET_PROP: String = "p"
  val Q_OFFSET_PROP: String = "q"

  // Conversions from ODB model values to TCS configuration values
  implicit class GuideWithOps(guideWith: StandardGuideOptions.Value) {
    val toProbeTracking: ProbeTrackingConfig = guideWith match {
      case StandardGuideOptions.Value.park => ProbeTrackingConfig.Parked
      case StandardGuideOptions.Value.freeze => ProbeTrackingConfig.Off
      case StandardGuideOptions.Value.guide => ProbeTrackingConfig.On(NodChopTrackingConfig.Normal)
    }

    val toGuideSensorOption: GuiderSensorOption = {
      if (guideWith.isActive) GuiderSensorOn
      else GuiderSensorOff
    }
  }

  @Lenses
  final case class TcsSeqConfig(
    guideWithP1: Option[StandardGuideOptions.Value],
    guideWithP2: Option[StandardGuideOptions.Value],
    guideWithOI: Option[StandardGuideOptions.Value],
    guideWithAO: Option[StandardGuideOptions.Value],
    offsetA: Option[InstrumentOffset],
    wavelA: Option[Wavelength],
    scienceFoldPosition: ScienceFoldPosition
  )

  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  object TcsSeqConfig

  def fromConfig(controller: TcsController, subsystems: NonEmptySet[Subsystem], gaos: Option[Either[Altair[IO],
    Gems[IO]]], guideConfigDb: GuideConfigDb[IO])(
    config: Config, scienceFoldPosition: ScienceFoldPosition, observingWavelength: Option[Wavelength]
  ): Tcs = {

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
      scienceFoldPosition
    )

    Tcs(controller, subsystems, gaos, guideConfigDb)(tcsSeqCfg)

  }

}
