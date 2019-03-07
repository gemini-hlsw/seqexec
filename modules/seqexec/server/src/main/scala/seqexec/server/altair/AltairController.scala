// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import cats.Eq
import cats.implicits._
import seqexec.server.tcs.Gaos.{PauseCondition, ResumeCondition}
import squants.Time

trait AltairController[F[_]] {
  import AltairController._

  def pause(reasons: Set[PauseCondition], fieldLens: FieldLens)(cfg: AltairConfig)
  : F[Set[ResumeCondition] => F[Unit]]

  def observe(expTime: Time)(cfg: AltairConfig): F[Unit]

  def endObserve(cfg: AltairConfig): F[Unit]

  def isFollowing: F[Option[Boolean]]
}

object AltairController {

  sealed trait AltairConfig

  case object AltairOff extends AltairConfig
  final case class Ngs(blend: Boolean) extends AltairConfig
  final case class Lgs(strap: Boolean, sfo: Boolean) extends AltairConfig
  case object LgsWithP1 extends AltairConfig
  case object LgsWithOi extends AltairConfig

  type FieldLens = edu.gemini.spModel.gemini.altair.AltairParams.FieldLens

  implicit val ngsEq: Eq[Ngs] = Eq.by(_.blend)
  implicit val lgsEq: Eq[Lgs] = Eq.by(x => (x.strap, x.sfo))

  implicit val eq: Eq[AltairConfig] = Eq.instance{
    case (AltairOff, AltairOff) => true
    case (a: Lgs, b: Lgs)       => a === b
    case (a: Ngs, b: Ngs)       => a === b
    case (LgsWithOi, LgsWithOi) => true
    case (LgsWithP1, LgsWithP1) => true
    case _                      => false
  }

}
