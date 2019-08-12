// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import cats.{Applicative, Eq}
import cats.implicits._
import seqexec.server.altair.AltairController.AltairConfig
import seqexec.server.gems.Gems.GemsWfsState
import seqexec.server.gems.GemsController.{GemsConfig, GemsOff}
import seqexec.server.tcs.Gaos
import seqexec.server.tcs.Gaos.{PauseConditionSet, PauseResume, ResumeConditionSet}
import squants.Time

trait Gems[F[_]] extends Gaos[F] {
  val cfg: GemsConfig

  def pauseResume(config: GemsConfig, pauseReasons: PauseConditionSet,
                  resumeReasons: ResumeConditionSet): F[PauseResume[F]]

  val stateGetter: GemsWfsState[F]

}

object Gems {

  private class GemsImpl[F[_]: Applicative] (controller: GemsController[F], config: GemsConfig) extends Gems[F] {

    override val cfg: GemsConfig = config

    override def observe(config: Either[AltairConfig, GemsConfig], expTime: Time): F[Unit] = controller.observe(expTime)

    override def endObserve(config: Either[AltairConfig, GemsConfig]): F[Unit] = controller.endObserve

    override def pauseResume(config: GemsConfig,
                             pauseReasons: PauseConditionSet,
                             resumeReasons: ResumeConditionSet): F[PauseResume[F]] = {
      controller.pauseResume(cfg, pauseReasons, resumeReasons)
    }

    override val stateGetter: GemsWfsState[F] = controller.stateGetter
  }

  def fromConfig[F[_]: Applicative](controller: GemsController[F])/*(config: Config)*/: F[Gems[F]] =
    Applicative[F].pure(new GemsImpl[F](controller, GemsOff))

  trait DetectorStateOps[T] {
    val trueVal: T
    val falseVal: T
  }

  object DetectorStateOps {
    def apply[T](implicit b: DetectorStateOps[T]): DetectorStateOps[T] = b

    def build[T](t: T, f: T): DetectorStateOps[T] = new DetectorStateOps[T] {
      override val trueVal: T = t
      override val falseVal: T = f
    }

    def fromBoolean[T: DetectorStateOps](b: Boolean): T =
      if(b) DetectorStateOps[T].trueVal else DetectorStateOps[T].falseVal

    def isActive[T: DetectorStateOps: Eq](v: T): Boolean = v === DetectorStateOps[T].trueVal
  }

  sealed trait Ngs1DetectorState extends Product with Serializable
  object Ngs1DetectorState {
    case object On extends Ngs1DetectorState
    case object Off extends Ngs1DetectorState

    implicit val ngs1DetectorStateEq: Eq[Ngs1DetectorState] = Eq.fromUniversalEquals
    implicit val ngs1DetectorStateDetectorStateOps: DetectorStateOps[Ngs1DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Ngs2DetectorState extends Product with Serializable
  object Ngs2DetectorState {
    case object On extends Ngs2DetectorState
    case object Off extends Ngs2DetectorState

    implicit val ngs2DetectorStateEq: Eq[Ngs2DetectorState] = Eq.fromUniversalEquals
    implicit val ngs2DetectorStateDetectorStateOps: DetectorStateOps[Ngs2DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Ngs3DetectorState extends Product with Serializable
  object Ngs3DetectorState {
    case object On extends Ngs3DetectorState
    case object Off extends Ngs3DetectorState

    implicit val ngs3DetectorStateEq: Eq[Ngs3DetectorState] = Eq.fromUniversalEquals
    implicit val ngs3DetectorStateDetectorStateOps: DetectorStateOps[Ngs3DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw1DetectorState extends Product with Serializable
  object Odgw1DetectorState {
    case object On extends Odgw1DetectorState
    case object Off extends Odgw1DetectorState

    implicit val odgw1DetectorStateEq: Eq[Odgw1DetectorState] = Eq.fromUniversalEquals
    implicit val odgw1DetectorStateDetectorStateOps: DetectorStateOps[Odgw1DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw2DetectorState extends Product with Serializable
  object Odgw2DetectorState {
    case object On extends Odgw2DetectorState
    case object Off extends Odgw2DetectorState

    implicit val odgw2DetectorStateEq: Eq[Odgw2DetectorState] = Eq.fromUniversalEquals
    implicit val odgw2DetectorStateDetectorStateOps: DetectorStateOps[Odgw2DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw3DetectorState extends Product with Serializable
  object Odgw3DetectorState {
    case object On extends Odgw3DetectorState
    case object Off extends Odgw3DetectorState

    implicit val odgw3DetectorStateEq: Eq[Odgw3DetectorState] = Eq.fromUniversalEquals
    implicit val odgw3DetectorStateDetectorStateOps: DetectorStateOps[Odgw3DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  sealed trait Odgw4DetectorState extends Product with Serializable
  object Odgw4DetectorState {
    case object On extends Odgw4DetectorState
    case object Off extends Odgw4DetectorState

    implicit val odgw4DetectorStateEq: Eq[Odgw4DetectorState] = Eq.fromUniversalEquals
    implicit val odgw4DetectorStateDetectorStateOps: DetectorStateOps[Odgw4DetectorState] =
      DetectorStateOps.build(On, Off)
  }

  final case class GemsWfsState[F[_]](
    ngs1: F[Option[Ngs1DetectorState]],
    ngs2: F[Option[Ngs2DetectorState]],
    ngs3: F[Option[Ngs3DetectorState]],
    odgw1: F[Option[Odgw1DetectorState]],
    odgw2: F[Option[Odgw2DetectorState]],
    odgw3: F[Option[Odgw3DetectorState]],
    odgw4: F[Option[Odgw4DetectorState]]
  )

}




