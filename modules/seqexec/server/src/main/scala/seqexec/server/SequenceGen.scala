// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

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
final case class SequenceGen[F[_]](id: Observation.Id, title: String,
                             instrument: Instrument,
                             steps: List[SequenceGen.StepGen[F]]) {
  val resources: Set[Resource] = steps.collect{
    case SequenceGen.PendingStepGen(_, _, resources, _) => resources
  }.foldMap(identity(_))

  def configActionCoord(stepId: StepId, r: Resource): Option[ActionCoordsInSeq] =
    steps.find(_.id === stepId)
      .collect{ case p @ SequenceGen.PendingStepGen(_, _, _, _) => p}
      .flatMap{ _.generator.configActionCoord(r) }
      .map{case (ex, ac) => ActionCoordsInSeq(stepId, ex, ac)}

  def resourceAtCoords(c: ActionCoordsInSeq): Option[Resource] =
    steps.find(_.id === c.stepId)
      .collect{ case p @ SequenceGen.PendingStepGen(_, _, _, _) => p}
      .flatMap(_.generator.resourceAtCoords(c.execIdx, c.actIdx))
}

object SequenceGen {

  sealed trait StepGen[F[_]] {
    val id: StepId
    val config: StepConfig

    def generate(ctx: HeaderExtraData): EngineStep[F] = this match {
      case PendingStepGen(_, _, _, g) => EngineStep.init[F](id, g.generate(ctx))
      case SkippedStepGen(id, _)      => EngineStep.init[F](id, Nil)
        .copy(skipped = EngineStep.Skipped(true))
      case CompletedStepGen(id, _, _) => EngineStep.init[F](id, Nil)
    }
  }

  final case class StepActionsGen[F[_]](pre: List[Actions[F]],
                                  configs: Map[Resource, Action[F]],
                                  post: HeaderExtraData => List[Actions[F]]) {
    def generate(ctx: HeaderExtraData): List[Actions[F]] =
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

  final case class PendingStepGen[F[_]](override val id: StepId,
                                  override val config: StepConfig,
                                  resources: Set[Resource],
                                  generator: StepActionsGen[F]
                             ) extends StepGen[F]


  final case class SkippedStepGen[F[_]](override val id: StepId,
                                  override val config: StepConfig
                              ) extends StepGen[F]

  // Receiving a sequence from the ODB with a completed step without an image file id would be
  // weird, but I still use an Option just in case
  final case class CompletedStepGen[F[_]](override val id: StepId,
                                    override val config: StepConfig,
                                    fileId: Option[ImageFileId]
                                ) extends StepGen[F]

}
