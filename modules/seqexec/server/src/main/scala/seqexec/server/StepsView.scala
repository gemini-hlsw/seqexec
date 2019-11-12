// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import cats.data.NonEmptyList
import seqexec.model.ActionType
import seqexec.model.Step
import seqexec.model.StandardStep
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.ActionStatus
import seqexec.model.enum.Resource
import seqexec.model.enum.Instrument
import seqexec.model.enum.Instrument._
import seqexec.model.StepState
import seqexec.engine
import seqexec.engine.Action.ActionState
import seqexec.engine.Action
import seqexec.engine.ParallelActions
import seqexec.model.NodAndShuffleStep.PendingObserveCmd
import seqexec.server.gmos.GmosStepsView

trait StepsView[F[_]] {
  /**
   * This method creates a view of the step for the client
   * The Step returned maybe a StandardStep of be specialized e.g. for N&S
   */
  def stepView(
    stepg: SequenceGen.StepGen[F],
    step: engine.Step[F],
    altCfgStatus: List[(Resource, ActionStatus)],
    pendingObsCmd: Option[PendingObserveCmd]
  ): Step
}

object StepsView {
  private def kindToResource(kind: ActionType): List[Resource] = kind match {
    case ActionType.Configure(r) => List(r)
    case _                       => Nil
  }

  def splitAfter[A](l: List[A])(p: A => Boolean): (List[A], List[A]) =
    l.splitAt(l.indexWhere(p) + 1)

  def separateActions[F[_]](ls: NonEmptyList[Action[F]]): (List[Action[F]], List[Action[F]]) =
    ls.toList.partition(_.state.runState match {
        case ActionState.Completed(_) => false
        case ActionState.Failed(_)    => false
        case _                        => true
      }
    )

  def actionsToResources[F[_]](s: NonEmptyList[Action[F]]): (List[Resource], List[Resource]) =
    separateActions(s).bimap(_.map(_.kind).flatMap(kindToResource), _.map(_.kind).flatMap(kindToResource))

  private[server] def configStatus[F[_]](executions: List[ParallelActions[F]]): List[(Resource, ActionStatus)] = {
    // Remove undefined actions
    val ex = executions.filter { !separateActions(_)._2.exists(_.kind === ActionType.Undefined) }
    // Split where at least one is running
    val (current, pending) = splitAfter(ex)(separateActions(_)._1.nonEmpty)

    // Calculate the state up to the current
    val configStatus = current.foldLeft(Map.empty[Resource, ActionStatus]) {
      case (s, e) =>
        val (a, r) = separateActions(e).bimap(
            _.flatMap(a => kindToResource(a.kind).tupleRight(ActionStatus.Running)).toMap,
            _.flatMap(r => kindToResource(r.kind).tupleRight(ActionStatus.Completed)).toMap)
        s ++ a ++ r
    }

    // Find out systems in the future
    val presentSystems = configStatus.keys.toList
    // Calculate status of pending items
    val systemsPending = pending
      .map(actionsToResources)
      .flatMap { case(a, b) => a.tupleRight(ActionStatus.Pending) ::: b.tupleRight(ActionStatus.Completed) }
      .filter {
        case (a, _) => !presentSystems.contains(a)
      }.distinct

    (configStatus ++ systemsPending).toList.sortBy(_._1)
  }

  /**
   * Calculates the config status for pending steps
   */
  private[server] def pendingConfigStatus[F[_]](executions: List[ParallelActions[F]]): List[(Resource, ActionStatus)] =
    executions
      .map(actionsToResources)
      .flatMap { case (a, b) => a ::: b }
      .distinct
      .tupleRight(ActionStatus.Pending).sortBy(_._1)

  /**
   * Overall pending status for a step
   */
  def stepConfigStatus[F[_]](step: engine.Step[F]): List[(Resource, ActionStatus)] =
    step.status match {
      case StepState.Pending => pendingConfigStatus(step.executions)
      case _                 => configStatus(step.executions)
    }

  private def observeAction[F[_]](executions: List[ParallelActions[F]]): Option[Action[F]] =
    // FIXME This is too naive and doesn't work properly for N&S
    executions.flatMap(_.toList).find(_.kind === ActionType.Observe)

  def observeStatus[F[_]](executions: List[ParallelActions[F]]): ActionStatus =
    observeAction(executions)
      .map(_.state.runState.actionStatus)
      .getOrElse(ActionStatus.Pending)

  def fileId[F[_]](executions: List[engine.ParallelActions[F]]): Option[ImageFileId] =
    observeAction(executions).flatMap(_.state.partials.collectFirst{
      case FileIdAllocated(fid) => fid
    })

  def defaultStepsView[F[_]]: StepsView[F] = new StepsView[F] {
    def stepView(
      stepg: SequenceGen.StepGen[F],
      step: engine.Step[F],
      altCfgStatus: List[(Resource, ActionStatus)],
      pendingObsCmd: Option[PendingObserveCmd]
    ): Step = {
      val status = step.status
      val configStatus =
        if (status.runningOrComplete) {
          stepConfigStatus(step)
        } else {
          altCfgStatus
        }

      StandardStep(
        id = step.id,
        config = stepg.config.toStepConfig,
        status = status,
        breakpoint = step.breakpoint.self,
        skip = step.skipMark.self,
        configStatus = configStatus,
        observeStatus = observeStatus(step.executions),
        fileId = fileId(step.executions).orElse(stepg.some.collect{
          case SequenceGen.CompletedStepGen(_, _, fileId) => fileId
        }.flatten))
    }

  }

  def stepsView[F[_]](instrument: Instrument): StepsView[F] = instrument match {
    case GmosN | GmosS => GmosStepsView.stepsView[F]
    case _             => defaultStepsView[F]
  }
}
