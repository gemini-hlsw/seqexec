// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.altair

import seqexec.server.tcs.Gaos.Reason
import squants.Time

trait AltairController[F[_]] {
  import AltairController._

  def pause(reasons: Set[Reason]): F[Unit]
  def resume(reasons: Set[Reason], config: AltairConfig): F[Unit]
  def observe(expTime: Time): F[Unit]
  def endObserve: F[Unit]
}

object AltairController {

  sealed trait AltairConfig

  case object AltairOff extends AltairConfig
  case object Ngs extends AltairConfig
  final case class Lgs(strap: Boolean, sfo: Boolean) extends AltairConfig
  case object LgsWithP1 extends AltairConfig
  final case class LgsWithOi(blend: Boolean) extends AltairConfig

}
