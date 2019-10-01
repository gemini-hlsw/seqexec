// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._
import cats.implicits._
import seqexec.model.enum._
import seqexec.model.GmosParameters._
import monocle.macros.Lenses
import squants.Time

@Lenses
final case class NodAndShuffleStatus(
  observing: ActionStatus,
  totalExposureTime: Time,
  nodExposureTime: Time,
  cycles: NsCycles
)

object NodAndShuffleStatus {

  implicit val equalNodAndShuffleStatus: Eq[NodAndShuffleStatus] =
    Eq.by(x => (x.observing, x.totalExposureTime, x.nodExposureTime, x.cycles))
}
