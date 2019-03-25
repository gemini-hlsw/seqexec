// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import seqexec.model.StepId

sealed trait StepsTableTypeSelection extends Product with Serializable

object StepsTableTypeSelection {
  case object StepsTableSelected extends StepsTableTypeSelection
  final case class StepConfigTableSelected(step: StepId)
      extends StepsTableTypeSelection

  implicit val eq: Eq[StepsTableTypeSelection] = Eq.instance {
    case (StepsTableSelected, StepsTableSelected) => true
    case (StepConfigTableSelected(a), StepConfigTableSelected(b)) => a === b
    case _ => false
  }

  def fromStepId(s: Option[StepId]): StepsTableTypeSelection = s match {
    case Some(i) => StepConfigTableSelected(i)
    case _       => StepsTableSelected
  }

}
