// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import seqexec.server.tcs.Gaos.{PauseReason, ResumeReason}
import squants.Time

trait AltairController[F[_]] {
  import AltairController._

  def pause(reasons: Set[PauseReason], fieldLens: FieldLens)(cfg: AltairConfig): F[Unit]
  def resume(reasons: Set[ResumeReason])(cfg: AltairConfig): F[Unit]
  def observe(expTime: Time)(cfg: AltairConfig): F[Unit]
  def endObserve(cfg: AltairConfig): F[Unit]
  def config(cfg: AltairConfig): F[Unit]
}

object AltairController {

  sealed trait AltairConfig

  case object AltairOff extends AltairConfig
  final case class Ngs(blend: Boolean) extends AltairConfig
  final case class Lgs(strap: Boolean, sfo: Boolean) extends AltairConfig
  case object LgsWithP1 extends AltairConfig
  case object LgsWithOi extends AltairConfig

  type FieldLens = edu.gemini.spModel.gemini.altair.AltairParams.FieldLens

}
