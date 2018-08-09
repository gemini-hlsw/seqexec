// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.{ Eq, Show }
import cats.implicits._

sealed abstract class StepType(val label: String)
  extends Product with Serializable

object StepType {

  case object Object      extends StepType("OBJECT")
  case object Arc         extends StepType("ARC")
  case object Flat        extends StepType("FLAT")
  case object Bias        extends StepType("BIAS")
  case object Dark        extends StepType("DARK")
  case object Calibration extends StepType("CAL")

  implicit val eq: Eq[StepType] =
    Eq.fromUniversalEquals

  implicit val show: Show[StepType] =
    Show.show(_.label)

  val all: List[StepType] =
    List(Object, Arc, Flat, Bias, Dark, Calibration)

  def fromString(s: String): Option[StepType] =
    all.find(_.label === s)

}
