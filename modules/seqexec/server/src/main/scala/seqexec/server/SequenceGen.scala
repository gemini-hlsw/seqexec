// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server


import cats.Eq
import cats.implicits._
import gem.Observation
import seqexec.model.Model.{Instrument, Resource, StepConfig}
import seqexec.engine.{Step => EngineStep}

final case class SequenceGen(id: Observation.Id, title: String, instrument: Instrument, steps: List[SequenceGen.Step]) {
  val resources: Set[Resource] = steps.foldMap(_.resources)
}

object SequenceGen {

  implicit val eq: Eq[SequenceGen] = Eq.by(x => (x.id))

  final case class Step(id: Int, config: StepConfig, resources: Set[Resource], generator: HeaderExtraData => EngineStep)

}
