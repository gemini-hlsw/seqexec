// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._
import seqexec.model.GmosParameters._
import seqexec.model.enum.NodAndShuffleStage
import seqexec.model.enum.NodAndShuffleStage._
import shapeless.tag

final case class NSSubexposure private (
  totalCycles: NsCycles, // Total amount of cycles for a N&S step
  cycle:       NsCycles, // Cycle for this sub exposure
  stageIndex:  Int,      // Nod or stage index (between 0 and 3)
  stage:       NodAndShuffleStage // Subexposure stage
) {
  val firstSubexposure: Boolean = (cycle: Int) === 0 && stageIndex === 0
  val lastSubexposure: Boolean  = (cycle: Int) === (totalCycles: Int) - 1 && stageIndex === NsSequence.length - 1
}

object NSSubexposure {
  implicit val eqNSSubexposure: Eq[NSSubexposure] =
    Eq.by(x => (x.totalCycles, x.cycle, x.stageIndex, x.stage))

  val Zero: NSSubexposure = NSSubexposure(tag[NsCyclesI][Int](0), tag[NsCyclesI][Int](0), 0, StageA)

  // Smart constructor returns a Some if the parameters are logically consistent
  def apply(
    totalCycles: NsCycles,
    cycle:       NsCycles,
    stageIndex:  Int
  ): Option[NSSubexposure] =
    if (totalCycles >= 0 && cycle >= 0 && cycle <= totalCycles && stageIndex >= 0 && stageIndex < NsSequence.length) {
      NSSubexposure(totalCycles, cycle, stageIndex, NsSequence.toList.lift(stageIndex).getOrElse(StageA)).some
    } else none

  // Calculate the subexposures
  def subexposures(
    totalCycles: Int
  ): List[NSSubexposure] =
    (for {
      i <- 0 until totalCycles
      j <- 0 until NsSequence.length
    } yield NSSubexposure(tag[NsCyclesI][Int](totalCycles), tag[NsCyclesI][Int](i), j)).toList.flatten

}
