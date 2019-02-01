// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.implicits._
import mouse.all._
import gem.Observation
import seqexec.engine.{Action, ActionCoordsInSeq, Actions, Step => EngineStep}
import seqexec.engine.ExecutionIndex
import seqexec.engine.ActionIndex
import seqexec.model.{StepConfig, StepId}
import seqexec.model.enum.{Instrument, Resource}
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

  def configActionCoord(stepId: StepId, r: Resource): Option[ActionCoordsInSeq] =
    steps.find(_.id === stepId).collect{ case p@SequenceGen.PendingStepGen(_, _, _, _) => p}
      .flatMap{ _.generator.configActionCoord(r) }
      .map{case (ex, ac) => ActionCoordsInSeq(stepId, r, ex, ac)}

  def resourceAtCoords(c: ActionCoordsInSeq): Option[Resource] = steps.find(_.id === c.stepId)
    .collect{ case p@SequenceGen.PendingStepGen(_, _, _, _) => p}
    .flatMap(_.generator.resourceAtCoords(c.execIdx, c.actIdx))
}

object SequenceGen {

  sealed trait StepGen {
    val id: StepId
    val config: StepConfig

    def generate(ctx: HeaderExtraData): EngineStep[IO] = this match {
      case PendingStepGen(_, _, _, g) => EngineStep.init(id, g.generate(ctx))
      case SkippedStepGen(id, _)      => EngineStep.init[IO](id, Nil).copy(skipped =
        EngineStep.Skipped(true))
      case CompletedStepGen(id, _, _) => EngineStep.init[IO](id, Nil)
    }
  }

  final case class StepActionsGen(pre: List[Actions[IO]],
                                  configs: Map[Resource, Action[IO]],
                                  post: HeaderExtraData => List[Actions[IO]]) {
    def generate(ctx: HeaderExtraData): List[Actions[IO]] =
      pre ++
        (if(configs.isEmpty) List() else List(configs.values.toList)) ++
        post(ctx)
    def configActionCoord(r: Resource): Option[(ExecutionIndex, ActionIndex)] = {
      val i = configs.keys.toIndexedSeq.indexOf(r)
      (i>=0).option(i)
        .map(i => (ExecutionIndex(pre.length.toLong), ActionIndex(i.toLong)))
    }
    def resourceAtCoords(ex: ExecutionIndex, ac: ActionIndex): Option[Resource] =
      if (ex.self === pre.length.toLong) configs.keys.toList.get(ac.self)
      else None

  }

  final case class PendingStepGen(override val id: StepId,
                                  override val config: StepConfig,
                                  resources: Set[Resource],
                                  generator: StepActionsGen
                             ) extends StepGen


  final case class SkippedStepGen(override val id: StepId,
                                  override val config: StepConfig
                              ) extends StepGen

  // Receiving a sequence from the ODB with a completed step without an image file id would be
  // weird, but I still use an Option just in case
  final case class CompletedStepGen(override val id: StepId,
                                    override val config: StepConfig,
                                    fileId: Option[ImageFileId]
                                ) extends StepGen

}
