// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gems

import seqexec.server.gems.Gems.GemsGuiderStatus
import seqexec.server.tcs.Gaos.{PauseCondition, PauseResume, ResumeCondition}
import squants.Time

trait GemsController[F[_]] {
  import GemsController._

  def pauseResume(config: GemsConfig, pauseReasons: Set[PauseCondition],
                  resumeReasons: Set[ResumeCondition]): F[PauseResume[F]]
  def observe(expTime: Time): F[Unit]
  def endObserve: F[Unit]

  val stateGetter: GemsGuiderStatus[F]

}

object GemsController {

  sealed trait GemsConfig

  case object GemsOff extends GemsConfig

  final case class GemsOn(
    ttgs1: Boolean,
    ttgs2: Boolean,
    ttgs3: Boolean,
    odgw1: Boolean,
    odgw2: Boolean,
    odgw3: Boolean,
    odgw4: Boolean,
    useOI: Boolean,
    useP1: Boolean
  ) extends GemsConfig

}
