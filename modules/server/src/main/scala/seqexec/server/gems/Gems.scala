// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.Applicative
import cats.Eq
import cats.MonadError
import cats.syntax.all._
import edu.gemini.spModel.gemini.gems.CanopusWfs
import edu.gemini.spModel.gemini.gsaoi.GsaoiOdgw
import edu.gemini.spModel.guide.StandardGuideOptions
import edu.gemini.spModel.target.obsComp.TargetObsCompConstants.GUIDE_WITH_OIWFS_PROP
import org.typelevel.log4cats.Logger
import seqexec.server.CleanConfig
import seqexec.server.CleanConfig.extractItem
import seqexec.server.ConfigUtilOps._
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.Gems.GemsWfsState
import seqexec.server.gems.GemsController.Cwfs1Usage
import seqexec.server.gems.GemsController.Cwfs2Usage
import seqexec.server.gems.GemsController.Cwfs3Usage
import seqexec.server.gems.GemsController.GemsConfig
import seqexec.server.gems.GemsController.OIUsage
import seqexec.server.gems.GemsController.Odgw1Usage
import seqexec.server.gems.GemsController.Odgw2Usage
import seqexec.server.gems.GemsController.Odgw3Usage
import seqexec.server.gems.GemsController.Odgw4Usage
import seqexec.server.gems.GemsController.P1Usage
import seqexec.server.tcs.{ Gaos, GuideConfig, GuideConfigDb, Tcs }
import seqexec.server.tcs.Gaos.{
  PauseCondition,
  PauseConditionSet,
  PauseResume,
  ResumeCondition,
  ResumeConditionSet
}
import squants.Time

trait Gems[F[_]] extends Gaos[F] {
  val cfg: GemsConfig

  def pauseResume(
    pauseReasons:  PauseConditionSet,
    resumeReasons: ResumeConditionSet
  ): F[PauseResume[F]]

  val stateGetter: GemsWfsState[F]

}

object Gems {

  private class GemsImpl[F[_]: MonadError[*[_], Throwable]](
    controller:    GemsController[F],
    config:        GemsConfig,
    guideConfigDb: GuideConfigDb[F]
  )(implicit L:    Logger[F])
      extends Gems[F] {

    override val cfg: GemsConfig = config

    override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] =
      ().pure[F]

    override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] = ().pure[F]

    override def pauseResume(
      pauseReasons:  PauseConditionSet,
      resumeReasons: ResumeConditionSet
    ): F[PauseResume[F]] =
      guideConfigDb.value.flatMap { g =>
        g.gaosGuide match {
          case Some(Right(gemsCfg)) =>
            val filteredPauseReasons  = filterPauseReasons(pauseReasons, g.gemsSkyPaused)
            val filteredResumeReasons = filterResumeReasons(
              resumeReasons,
              g.gemsSkyPaused || filteredPauseReasons.contains(PauseCondition.GaosGuideOff)
            )
            controller
              .pauseResume(filteredPauseReasons, filteredResumeReasons)(combine(gemsCfg, cfg))
              .map(x =>
                PauseResume(
                  x.pause.map(
                    _.flatMap(_ =>
                      guideConfigDb
                        .update(GuideConfig.gemsSkyPaused.replace(true))
                        .whenA(filteredPauseReasons.contains(PauseCondition.GaosGuideOff))
                    )
                  ),
                  x.resume.map(
                    _.flatMap(_ =>
                      guideConfigDb
                        .update(GuideConfig.gemsSkyPaused.replace(false))
                        .whenA(filteredResumeReasons.contains(ResumeCondition.GaosGuideOn))
                    )
                  )
                )
              )
          case _                    =>
            // If there is no configuration coming from TCC we just ignore it. This is the case when taking dome flats
            // We check in TcsSouth.scala that it is not an error
            L.info(
              "No GeMS guide configuration from TCC. GeMS control skipped for unguided step."
            ) *>
              PauseResume[F](none, none).pure[F]
        }
      }

    override val stateGetter: GemsWfsState[F] = controller.stateGetter
  }

  // Ignore GaosGuideOff if it was already sent in a previous step
  // TODO: do the same for Filter pause condition (for GSAOI calibrations)
  private def filterPauseReasons(
    pauseReasons: PauseConditionSet,
    isSkyPaused:  Boolean
  ): PauseConditionSet =
    if (isSkyPaused) pauseReasons.copy(fixed = pauseReasons.fixed - PauseCondition.GaosGuideOff)
    else pauseReasons

  private def filterResumeReasons(
    resumeReasons: ResumeConditionSet,
    isSkyPaused:   Boolean
  ): ResumeConditionSet =
    if (!isSkyPaused) resumeReasons.copy(fixed = resumeReasons.fixed - ResumeCondition.GaosGuideOn)
    else resumeReasons

  // `combine` calculates the final configuration between the configuration coming from the step and the configuration
  // set by the operator.
  private def combine(opConfig: GemsConfig, stepConfig: GemsConfig): GemsConfig =
    GemsController.GemsOn(
      Cwfs1Usage.fromBoolean(opConfig.isCwfs1Used && stepConfig.isCwfs1Used),
      Cwfs2Usage.fromBoolean(opConfig.isCwfs2Used && stepConfig.isCwfs2Used),
      Cwfs3Usage.fromBoolean(opConfig.isCwfs3Used && stepConfig.isCwfs3Used),
      Odgw1Usage.fromBoolean(opConfig.isOdgw1Used && stepConfig.isOdgw1Used),
      Odgw2Usage.fromBoolean(opConfig.isOdgw2Used && stepConfig.isOdgw2Used),
      Odgw3Usage.fromBoolean(opConfig.isOdgw3Used && stepConfig.isOdgw3Used),
      Odgw4Usage.fromBoolean(opConfig.isOdgw4Used && stepConfig.isOdgw4Used),
      P1Usage.fromBoolean(opConfig.isP1Used && stepConfig.isP1Used),
      OIUsage.fromBoolean(opConfig.isOIUsed && stepConfig.isOIUsed)
    )

  def fromConfig[F[_]: MonadError[*[_], Throwable]: Logger](
    guideConfigDb: GuideConfigDb[F],
    config:        CleanConfig
  ): F[GemsController[F] => Gems[F]] = {
    for {
      p1    <- config.extractTelescopeAs[StandardGuideOptions.Value](Tcs.GUIDE_WITH_PWFS1_PROP)
      oi     = config.extractTelescopeAs[StandardGuideOptions.Value](GUIDE_WITH_OIWFS_PROP).toOption
      cwfs1 <-
        config.extractTelescopeAs[StandardGuideOptions.Value](CanopusWfs.cwfs1.getSequenceProp)
      cwfs2 <-
        config.extractTelescopeAs[StandardGuideOptions.Value](CanopusWfs.cwfs2.getSequenceProp)
      cwfs3 <-
        config.extractTelescopeAs[StandardGuideOptions.Value](CanopusWfs.cwfs3.getSequenceProp)
      odgw1 <-
        config.extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw1.getSequenceProp)
      odgw2 <-
        config.extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw2.getSequenceProp)
      odgw3 <-
        config.extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw3.getSequenceProp)
      odgw4 <-
        config.extractTelescopeAs[StandardGuideOptions.Value](GsaoiOdgw.odgw4.getSequenceProp)
    } yield { c: GemsController[F] =>
      new GemsImpl[F](
        c,
        GemsController.GemsOn(
          Cwfs1Usage.fromBoolean(cwfs1.isActive),
          Cwfs2Usage.fromBoolean(cwfs2.isActive),
          Cwfs3Usage.fromBoolean(cwfs3.isActive),
          Odgw1Usage.fromBoolean(odgw1.isActive),
          Odgw2Usage.fromBoolean(odgw2.isActive),
          Odgw3Usage.fromBoolean(odgw3.isActive),
          Odgw4Usage.fromBoolean(odgw4.isActive),
          P1Usage.fromBoolean(p1.isActive),
          OIUsage.fromBoolean(oi.exists(_.isActive))
        ),
        guideConfigDb
      ): Gems[F]
    }
  }.toF[F]

  trait DetectorStateOps[T] {
    val trueVal: T
    val falseVal: T
  }

  object DetectorStateOps {
    def apply[T](implicit b: DetectorStateOps[T]): DetectorStateOps[T] = b

    def build[T](t: T, f: T): DetectorStateOps[T] = new DetectorStateOps[T] {
      override val trueVal: T  = t
      override val falseVal: T = f
    }

    def fromBoolean[T: DetectorStateOps](b: Boolean): T =
      if (b) DetectorStateOps[T].trueVal else DetectorStateOps[T].falseVal

    def isActive[T: DetectorStateOps: Eq](v: T): Boolean = v === DetectorStateOps[T].trueVal
  }

  sealed trait Cwfs1DetectorState extends Product with Serializable
  object Cwfs1DetectorState {
    case object On  extends Cwfs1DetectorState
    case object Off extends Cwfs1DetectorState

    implicit val cwfs1DetectorStateEq: Eq[Cwfs1DetectorState]                             = Eq.fromUniversalEquals
    implicit val cwfs1DetectorStateDetectorStateOps: DetectorStateOps[Cwfs1DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Cwfs2DetectorState extends Product with Serializable
  object Cwfs2DetectorState {
    case object On  extends Cwfs2DetectorState
    case object Off extends Cwfs2DetectorState

    implicit val cwfs2DetectorStateEq: Eq[Cwfs2DetectorState]                             = Eq.fromUniversalEquals
    implicit val cwfs2DetectorStateDetectorStateOps: DetectorStateOps[Cwfs2DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Cwfs3DetectorState extends Product with Serializable
  object Cwfs3DetectorState {
    case object On  extends Cwfs3DetectorState
    case object Off extends Cwfs3DetectorState

    implicit val cwfs3DetectorStateEq: Eq[Cwfs3DetectorState]                             = Eq.fromUniversalEquals
    implicit val cwfs3DetectorStateDetectorStateOps: DetectorStateOps[Cwfs3DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw1DetectorState extends Product with Serializable
  object Odgw1DetectorState {
    case object On  extends Odgw1DetectorState
    case object Off extends Odgw1DetectorState

    implicit val odgw1DetectorStateEq: Eq[Odgw1DetectorState]                             = Eq.fromUniversalEquals
    implicit val odgw1DetectorStateDetectorStateOps: DetectorStateOps[Odgw1DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw2DetectorState extends Product with Serializable
  object Odgw2DetectorState {
    case object On  extends Odgw2DetectorState
    case object Off extends Odgw2DetectorState

    implicit val odgw2DetectorStateEq: Eq[Odgw2DetectorState]                             = Eq.fromUniversalEquals
    implicit val odgw2DetectorStateDetectorStateOps: DetectorStateOps[Odgw2DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw3DetectorState extends Product with Serializable
  object Odgw3DetectorState {
    case object On  extends Odgw3DetectorState
    case object Off extends Odgw3DetectorState

    implicit val odgw3DetectorStateEq: Eq[Odgw3DetectorState]                             = Eq.fromUniversalEquals
    implicit val odgw3DetectorStateDetectorStateOps: DetectorStateOps[Odgw3DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw4DetectorState extends Product with Serializable
  object Odgw4DetectorState {
    case object On  extends Odgw4DetectorState
    case object Off extends Odgw4DetectorState

    implicit val odgw4DetectorStateEq: Eq[Odgw4DetectorState]                             = Eq.fromUniversalEquals
    implicit val odgw4DetectorStateDetectorStateOps: DetectorStateOps[Odgw4DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  final case class GemsWfsState[F[_]](
    cwfs1: F[Cwfs1DetectorState],
    cwfs2: F[Cwfs2DetectorState],
    cwfs3: F[Cwfs3DetectorState],
    odgw1: F[Odgw1DetectorState],
    odgw2: F[Odgw2DetectorState],
    odgw3: F[Odgw3DetectorState],
    odgw4: F[Odgw4DetectorState]
  )

  object GemsWfsState {
    def allOff[F[_]: Applicative]: GemsWfsState[F] = GemsWfsState(
      Cwfs1DetectorState.Off.pure[F].widen[Cwfs1DetectorState],
      Cwfs2DetectorState.Off.pure[F].widen[Cwfs2DetectorState],
      Cwfs3DetectorState.Off.pure[F].widen[Cwfs3DetectorState],
      Odgw1DetectorState.Off.pure[F].widen[Odgw1DetectorState],
      Odgw2DetectorState.Off.pure[F].widen[Odgw2DetectorState],
      Odgw3DetectorState.Off.pure[F].widen[Odgw3DetectorState],
      Odgw4DetectorState.Off.pure[F].widen[Odgw4DetectorState]
    )
  }

}
