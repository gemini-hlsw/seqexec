// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import seqexec.model.StepId

sealed trait StepsTableTypeSelection extends Product with Serializable

object StepsTableTypeSelection {
  case object StepsTableSelected extends StepsTableTypeSelection
  final case class StepConfigTableSelected(step: StepId)
      extends StepsTableTypeSelection

  implicit val eq: Eq[StepsTableTypeSelection] = Eq.fromUniversalEquals

  def fromStepId(s: Option[StepId]): StepsTableTypeSelection = s match {
    case Some(i) => StepConfigTableSelected(i)
    case _       => StepsTableSelected
  }

}
