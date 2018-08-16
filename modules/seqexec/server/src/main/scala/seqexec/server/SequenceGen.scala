// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server


import cats.Eq
import gem.Observation
import seqexec.model.Model.{Instrument, StepConfig}
import seqexec.engine.{Step => EngineStep}

final case class SequenceGen(id: Observation.Id, title: String, instrument: Instrument, steps: List[SequenceGen.Step])

object SequenceGen {

  implicit val eq: Eq[SequenceGen] = Eq.by(x => (x.id))

  final case class Step(id: Int, config: StepConfig, generator: HeaderExtraData => EngineStep)

}
