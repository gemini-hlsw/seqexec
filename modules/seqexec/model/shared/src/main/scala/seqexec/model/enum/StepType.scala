// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats._
import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed abstract class StepType(val label: String) extends Product with Serializable

object StepType {

  case object Object            extends StepType("OBJECT")
  case object Arc               extends StepType("ARC")
  case object Flat              extends StepType("FLAT")
  case object Bias              extends StepType("BIAS")
  case object Dark              extends StepType("DARK")
  case object Calibration       extends StepType("CAL")
  case object AlignAndCalib     extends StepType("A & C")
  case object NodAndShuffle     extends StepType("N & S")
  case object NodAndShuffleDark extends StepType("N&S DARK")

  implicit val show: Show[StepType] =
    Show.show(_.label)

  def fromString(s: String): Option[StepType] =
    StepTypeEnumerated.all.find(_.label === s)

  /** @group Typeclass Instances */
  implicit val StepTypeEnumerated: Enumerated[StepType] =
    Enumerated.of(Object,
                  Arc,
                  Flat,
                  Bias,
                  Dark,
                  Calibration,
                  AlignAndCalib,
                  NodAndShuffle,
                  NodAndShuffleDark
    )

}
