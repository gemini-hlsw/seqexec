// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.data.NonEmptyList
import lucuma.core.util.Enumerated

sealed trait NodAndShuffleStage extends Product with Serializable {
  val symbol: Symbol
}

// The OT lets beams up to G but in practice it is always A/B
object NodAndShuffleStage {
  case object StageA extends NodAndShuffleStage {
    val symbol: Symbol = Symbol("A")
  }
  case object StageB extends NodAndShuffleStage {
    val symbol: Symbol = Symbol("B")
  }
  case object StageC extends NodAndShuffleStage {
    val symbol: Symbol = Symbol("C")
  }
  case object StageD extends NodAndShuffleStage {
    val symbol: Symbol = Symbol("D")
  }
  case object StageE extends NodAndShuffleStage {
    val symbol: Symbol = Symbol("E")
  }
  case object StageF extends NodAndShuffleStage {
    val symbol: Symbol = Symbol("F")
  }
  case object StageG extends NodAndShuffleStage {
    val symbol: Symbol = Symbol("G")
  }

  /** @group Typeclass Instances */
  implicit val NSStageEnumerated: Enumerated[NodAndShuffleStage] =
    Enumerated.of(StageA, StageB, StageC, StageD, StageE, StageF, StageG)

  // The sequence of nod and shuffle is always BAAB,
  // In principle we'd expect the OT to send the sequence but instead the
  // sequence is hardcoded in the seqexec and we only read the positions from
  // the OT
  val NsSequence: NonEmptyList[NodAndShuffleStage] =
    NonEmptyList.of(StageB, StageA, StageA, StageB)

}
