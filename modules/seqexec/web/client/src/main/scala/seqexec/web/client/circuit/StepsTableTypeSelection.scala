// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import gem.Observation
import monocle.Getter
import seqexec.web.client.model._

sealed trait StepsTableTypeSelection extends Product with Serializable

object StepsTableTypeSelection {
  case object StepsTableSelected extends StepsTableTypeSelection
  case object StepConfigTableSelected extends StepsTableTypeSelection

  implicit val eq: Eq[StepsTableTypeSelection] = Eq.fromUniversalEquals

  def stepsTableTypeG(
    id: Observation.Id
  ): Getter[SeqexecAppRootModel, Option[StepsTableTypeSelection]] =
    SeqexecAppRootModel.sequencesOnDisplayL.composeGetter(
      SequencesOnDisplay.tabG(id)) >>> {
      _.map {
        case SeqexecTabActive(tab, _) =>
          tab.selectedStep
            .as(StepConfigTableSelected)
            .getOrElse(StepsTableSelected)
      }
    }
}
