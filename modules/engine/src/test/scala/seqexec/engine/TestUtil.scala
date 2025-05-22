// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.effect.IO
import monocle.Optional
import monocle.function.Index._
import monocle.macros.Lenses
import seqexec.model.Observation

object TestUtil {
  @Lenses
  final case class TestState(sequences: Map[Observation.Id, Sequence.State[IO]])

  object TestState extends Engine.State[IO, TestState] {
    override def sequenceStateIndex(sid: Observation.Id): Optional[TestState, Sequence.State[IO]] =
      TestState.sequences.andThen(mapIndex[Observation.Id, Sequence.State[IO]].index(sid))

  }
}
