// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import monocle.macros.Lenses
import seqexec.model.GmosParameters._
import seqexec.model.enum._
import squants.Time

final case class NSRunningState(action: NSAction, sub: NSSubexposure)

object NSRunningState {
  implicit val equalNSRunningState: Eq[NSRunningState] =
    Eq.by(x => (x.action, x.sub))
}

@Lenses
final case class NodAndShuffleStatus(
  observing: ActionStatus,
  totalExposureTime: Time,
  nodExposureTime: Time,
  cycles: NsCycles,
  state: Option[NSRunningState]
)

object NodAndShuffleStatus {

  implicit val equalNodAndShuffleStatus: Eq[NodAndShuffleStatus] =
    Eq.by(x => (x.observing, x.totalExposureTime, x.nodExposureTime, x.cycles, x.state))
}
