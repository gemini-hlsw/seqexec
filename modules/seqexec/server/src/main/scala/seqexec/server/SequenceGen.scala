// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.implicits._
import gem.Observation
import seqexec.model.StepConfig
import seqexec.model.enum.{Instrument, Resource}
import seqexec.engine.{Step => EngineStep}
import seqexec.model.dhs.ImageFileId

/*
 * SequenceGen keeps all the information extracted from the ODB sequence.
 * It is combined with header parameters to build an engine.Sequence. It allows to rebuild the
 * engine sequence whenever any of those parameters change.
 */
final case class SequenceGen(id: Observation.Id, title: String,
                             instrument: Instrument,
                             steps: List[SequenceGen.StepGen]) {
  val resources: Set[Resource] = steps.collect{
    case SequenceGen.PendingStepGen(_, _, resources, _) => resources
  }.foldMap(identity(_))
}

object SequenceGen {

  sealed trait StepGen {
    val id: Int
    val config: StepConfig

    def generate(ctx: HeaderExtraData): EngineStep[IO] = this match {
      case PendingStepGen(_, _, _, g) => g(ctx)
      case SkippedStepGen(id, _)      => EngineStep.init[IO](id, Nil).copy(skipped =
        EngineStep.Skipped(true))
      case CompletedStepGen(id, _, _) => EngineStep.init[IO](id, Nil)
    }
  }

  final case class PendingStepGen(override val id: Int,
                                  override val config: StepConfig,
                                  resources: Set[Resource],
                                  generator: HeaderExtraData => EngineStep[IO]
                             ) extends StepGen

  final case class SkippedStepGen(override val id: Int,
                                  override val config: StepConfig
                              ) extends StepGen

  // Receiving a sequence from the ODB with a completed step without an image file id would be
  // weird, but I still use an Option just in case
  final case class CompletedStepGen(override val id: Int,
                                    override val config: StepConfig,
                                    fileId: Option[ImageFileId]
                                ) extends StepGen

}
