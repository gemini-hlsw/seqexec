// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Eq
import cats.Show
import cats.syntax.all._
import seqexec.model.`enum`.Instrument
import seqexec.server.tcs.Gaos.GuideCapabilities
import seqexec.server.tcs.Gaos.PauseConditionSet
import seqexec.server.tcs.Gaos.ResumeConditionSet
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import squants.Time
import squants.space.Length

trait AltairController[F[_]] {
  import AltairController._

  def pauseResume(
    pauseReasons:  PauseConditionSet,
    resumeReasons: ResumeConditionSet,
    currentOffset: FocalPlaneOffset,
    instrument:    Instrument
  )(cfg: AltairConfig): F[AltairPauseResume[F]]

  def observe(expTime: Time)(cfg: AltairConfig): F[Unit]

  def endObserve(cfg: AltairConfig): F[Unit]

  def isFollowing: F[Boolean]

}

object AltairController {

  sealed trait AltairConfig

  case object AltairOff                                                         extends AltairConfig
  final case class Ngs(blend: Boolean, starPos: (Length, Length))               extends AltairConfig
  final case class Lgs(strap: Boolean, sfo: Boolean, starPos: (Length, Length)) extends AltairConfig
  case object LgsWithP1                                                         extends AltairConfig
  case object LgsWithOi                                                         extends AltairConfig

  type FieldLens = edu.gemini.spModel.gemini.altair.AltairParams.FieldLens

  implicit val ngsEq: Eq[Ngs] = Eq.by(_.blend)
  implicit val lgsEq: Eq[Lgs] = Eq.by(x => (x.strap, x.sfo))

  implicit val eq: Eq[AltairConfig] = Eq.instance {
    case (AltairOff, AltairOff) => true
    case (a: Lgs, b: Lgs)       => a === b
    case (a: Ngs, b: Ngs)       => a === b
    case (LgsWithOi, LgsWithOi) => true
    case (LgsWithP1, LgsWithP1) => true
    case _                      => false
  }

  implicit val showAltairConfig: Show[AltairConfig] = Show.fromToString[AltairConfig]

  sealed case class AltairPauseResume[F[_]](
    pause:             Option[F[Unit]],
    guideWhilePaused:  GuideCapabilities,
    pauseTargetFilter: Boolean,
    resume:            Option[F[Unit]],
    restoreOnResume:   GuideCapabilities,
    config:            Option[F[Unit]],
    forceFreeze:       Boolean
  )

}
