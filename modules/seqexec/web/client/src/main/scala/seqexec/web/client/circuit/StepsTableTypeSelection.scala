// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import seqexec.model.StepId
import seqexec.web.client.model._

sealed trait StepsTableTypeSelection extends Product with Serializable

object StepsTableTypeSelection {
  case object StepsTableSelected extends StepsTableTypeSelection
  final case class StepConfigTableSelected(step: Int)
      extends StepsTableTypeSelection

  implicit val eq: Eq[StepsTableTypeSelection] = Eq.fromUniversalEquals

  def fromStepId(s: Option[StepId]): StepsTableTypeSelection = s match {
    case Some(i) => StepConfigTableSelected(i)
    case _       => StepsTableSelected
  }

  def stepsTableTypeG(
    id: Observation.Id
  ): Getter[SeqexecAppRootModel, Option[StepsTableTypeSelection]] =
    SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
      SequencesOnDisplay.tabG(id)) >>> {
      _.map {
        case SeqexecTabActive(tab, _) =>
          tab.stepConfigDisplayed
            .map(StepConfigTableSelected.apply)
            .getOrElse(StepsTableSelected)
      }
    }
}
