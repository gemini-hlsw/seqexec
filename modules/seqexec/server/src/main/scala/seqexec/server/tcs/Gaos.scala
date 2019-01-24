// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import seqexec.server.SeqActionF
import seqexec.server.tcs.TcsController.FocalPlaneOffset
import squants.Time

/*
 * Interface to control AO systems. Implemented by Altair and GeMS
 */
trait Gaos[F[_]] {
  import Gaos._

  /*
   * Pause GAOS guiding. The GAOS system decides what to pause, according to the reasons given
   */
  def pause(reasons: Set[Reason]): SeqActionF[F, Unit]
  /*
   * Resume GAOS guiding. The GAOS system decides what to resume, according to the reasons given
   *
   */
  def resume(reasons: Set[Reason]): SeqActionF[F, Unit]

  /*
   * Notify GAOS system of the start of the observation
   */
  def observe(expTime: Time): SeqActionF[F, Unit]
  /*
   * Notify GAOS system of the end of the observation
   */
  def endObserve: SeqActionF[F, Unit]

}

object Gaos {
  sealed trait Reason
  // Telescope offset will be applied/was applied
  final case class BecauseOfOffset(offset: FocalPlaneOffset) extends Reason
  // OI will be turn off, was turn on
  case object BecauseOfOiChange extends Reason
  // PI will be turn off, was turn on
  case object BecauseOfP1Change extends Reason
}
