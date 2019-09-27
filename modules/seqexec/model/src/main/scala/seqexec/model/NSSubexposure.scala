// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.NodAndShuffleStage._

final case class NSSubexposure(
  totalCycles: Int,
  cycle:       Int,
  id:          Int,
  stage:       NodAndShuffleStage
) {
  val firstSubexposure = cycle === 0 && id === 0
  val lastSubexposure  = cycle === totalCycles - 1 && id === NsSequence.length - 1
}

object NSSubexposure {
  implicit val eqNSSubexposure: Eq[NSSubexposure] =
    Eq.by(x => (x.totalCycles, x.cycle, x.id, x.stage))

  // Calculate the subexposures
  def subexposures(
    cycles: Int
  ): List[NSSubexposure] =
    (for {
      i <- 0 until cycles
      j <- 0 until NsSequence.length
      stage = NsSequence.toList.lift(j).getOrElse(StageA)
    } yield NSSubexposure(cycles, i, j, stage)).toList

}
