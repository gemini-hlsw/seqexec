// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import gem.Observation
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.extra.Reusability
import seqexec.model.enum.Instrument
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.circuit.StepsTableFocus
import seqexec.model.{ Step, StepConfig }

package object reusability {
  implicit val instrumentReuse: Reusability[Instrument] = Reusability.byEq
  implicit val obsIdReuse: Reusability[Observation.Id] = Reusability.byEq
  implicit val stepConfigReuse: Reusability[StepConfig] = Reusability.byEq
  implicit val stepReuse: Reusability[Step] = Reusability.byEq
  implicit val clientStatusReuse: Reusability[ClientStatus] = Reusability.byEq
  implicit val stepsTableFocusReuse: Reusability[StepsTableFocus] = Reusability.byEq
}
