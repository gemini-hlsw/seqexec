// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.implicits._
import gem.Observation
import seqexec.model.StepConfig
import seqexec.model.enum.{Instrument, Resource}
import seqexec.engine.{Step => EngineStep}

/*
 * SequenceGen keeps all the information extracted from the ODB sequence.
 * It is combined with header parameters to build an engine.Sequence. It allows to rebuild the engine
 * whenever any of those parameters change.
 */
final case class SequenceGen(id: Observation.Id, title: String, instrument: Instrument, steps: List[SequenceGen.Step]) {
  val resources: Set[Resource] = steps.foldMap(_.resources)
}

object SequenceGen {

  final case class Step(id: Int, config: StepConfig, resources: Set[Resource],
                        generator: HeaderExtraData => EngineStep[IO])

}
