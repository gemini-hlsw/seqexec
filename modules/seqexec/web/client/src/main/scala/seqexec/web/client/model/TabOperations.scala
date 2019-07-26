// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import gem.util.Enumerated
import monocle.Lens
import monocle.macros.Lenses
import monocle.function.At.at
import monocle.function.At.atSortedMap
import seqexec.model.enum.Resource
import seqexec.model.StepId
import scala.collection.immutable.SortedMap

sealed trait RunOperation extends Product with Serializable
object RunOperation {
  case object RunIdle extends RunOperation
  case object RunInFlight extends RunOperation

  /** @group Typeclass Instances */
  implicit val RunOperationEnumerated: Enumerated[RunOperation] =
    Enumerated.of(RunIdle, RunInFlight)

}

sealed trait StopOperation extends Product with Serializable
object StopOperation {
  case object StopIdle extends StopOperation
  case object StopInFlight extends StopOperation

  /** @group Typeclass Instances */
  implicit val StopOperationEnumerated: Enumerated[StopOperation] =
    Enumerated.of(StopIdle, StopInFlight)

}

sealed trait AbortOperation extends Product with Serializable
object AbortOperation {
  case object AbortIdle extends AbortOperation
  case object AbortInFlight extends AbortOperation

  /** @group Typeclass Instances */
  implicit val AbortOperationEnumerated: Enumerated[AbortOperation] =
    Enumerated.of(AbortIdle, AbortInFlight)

}

sealed trait PauseOperation extends Product with Serializable
object PauseOperation {
  case object PauseIdle extends PauseOperation
  case object PauseInFlight extends PauseOperation

  /** @group Typeclass Instances */
  implicit val PauseOperationEnumerated: Enumerated[PauseOperation] =
    Enumerated.of(PauseIdle, PauseInFlight)

}

sealed trait CancelPauseOperation extends Product with Serializable
object CancelPauseOperation {
  case object CancelPauseIdle extends CancelPauseOperation
  case object CancelPauseInFlight extends CancelPauseOperation

  /** @group Typeclass Instances */
  implicit val CancelPauseOperationEnumerated: Enumerated[CancelPauseOperation] =
    Enumerated.of(CancelPauseIdle, CancelPauseInFlight)

}

sealed trait ResumeOperation extends Product with Serializable
object ResumeOperation {
  case object ResumeIdle extends ResumeOperation
  case object ResumeInFlight extends ResumeOperation

  /** @group Typeclass Instances */
  implicit val ResumeOperationEnumerated: Enumerated[ResumeOperation] =
    Enumerated.of(ResumeIdle, ResumeInFlight)

}

sealed trait SyncOperation extends Product with Serializable
object SyncOperation {
  case object SyncIdle extends SyncOperation
  case object SyncInFlight extends SyncOperation

  /** @group Typeclass Instances */
  implicit val SyncOperationEnumerated: Enumerated[SyncOperation] =
    Enumerated.of(SyncIdle, SyncInFlight)

}

sealed trait ResourceRunOperation extends Product with Serializable

object ResourceRunOperation {
  case object ResourceRunIdle extends ResourceRunOperation
  final case class ResourceRunInFlight(stepId: StepId) extends ResourceRunOperation
  final case class ResourceRunCompleted(stepId: StepId) extends ResourceRunOperation
  final case class ResourceRunFailed(stepId: StepId) extends ResourceRunOperation

  implicit val eqResourceRunOperation: Eq[ResourceRunOperation] = Eq.instance {
    case (ResourceRunIdle, ResourceRunIdle)                 => true
    case (ResourceRunInFlight(a), ResourceRunInFlight(b))   => a === b
    case (ResourceRunCompleted(a), ResourceRunCompleted(b)) => a === b
    case (ResourceRunFailed(a), ResourceRunFailed(b))       => a === b
    case _                                                  => false
  }
}

sealed trait StartFromOperation extends Product with Serializable
object StartFromOperation {
  case object StartFromInFlight extends StartFromOperation
  case object StartFromIdle extends StartFromOperation

  /** @group Typeclass Instances */
  implicit val StartFromOperationEnumerated: Enumerated[StartFromOperation] =
    Enumerated.of(StartFromIdle, StartFromInFlight)

}

/**
  * Hold transient states while excuting an operation
  */
@Lenses
final case class TabOperations(
  runRequested:         RunOperation,
  syncRequested:        SyncOperation,
  pauseRequested:       PauseOperation,
  cancelPauseRequested: CancelPauseOperation,
  resumeRequested:      ResumeOperation,
  stopRequested:        StopOperation,
  abortRequested:       AbortOperation,
  startFromRequested:   StartFromOperation,
  resourceRunRequested: SortedMap[Resource, ResourceRunOperation]
) {
  // Indicate if any resource is being executed
  def resourceInFlight(id: StepId): Boolean =
    resourceRunRequested.exists(_._2 match {
      case ResourceRunOperation.ResourceRunInFlight(sid) if sid === id =>
        true
      case _ => false
    })

  // Indicate if any resource is being executed
  def resourceInError(id: StepId): Boolean =
    resourceRunRequested.exists(_._2 match {
      case ResourceRunOperation.ResourceRunFailed(sid) if sid === id =>
        true
      case _ => false
    })

  def anyResourceInFlight: Boolean =
    resourceRunRequested.exists(_._2 match {
      case ResourceRunOperation.ResourceRunInFlight(_) =>
        true
      case _ => false
    })

  val stepRequestInFlight: Boolean =
    pauseRequested === PauseOperation.PauseInFlight ||
      cancelPauseRequested === CancelPauseOperation.CancelPauseInFlight ||
      resumeRequested === ResumeOperation.ResumeInFlight ||
      stopRequested === StopOperation.StopInFlight ||
      abortRequested === AbortOperation.AbortInFlight ||
      startFromRequested === StartFromOperation.StartFromInFlight
}

object TabOperations {
  implicit val eq: Eq[TabOperations] =
    Eq.by(
      x =>
        (x.runRequested,
         x.syncRequested,
         x.pauseRequested,
         x.cancelPauseRequested,
         x.resumeRequested,
         x.stopRequested,
         x.abortRequested,
         x.startFromRequested,
         x.resourceRunRequested)
    )

  def resourceRun(
    r: Resource
  ): Lens[TabOperations, Option[ResourceRunOperation]] =
    TabOperations.resourceRunRequested ^|-> at(r)

  // Set the resource operations in the map to idle.
  def clearAllResourceOperations: TabOperations => TabOperations =
    TabOperations.resourceRunRequested.modify(_.map {
      case (r, _) => r -> ResourceRunOperation.ResourceRunIdle
    })

  // Set the resource operations in the map to idle.
  def clearResourceOperations(re: Resource): TabOperations => TabOperations =
    TabOperations.resourceRunRequested.modify(_.map {
      case (r, _) if re === r => r -> ResourceRunOperation.ResourceRunIdle
      case r                  => r
    })

  // Set the resource operations in the map to idle.
  def clearCommonResourceCompleted(
    re: Resource
  ): TabOperations => TabOperations =
    TabOperations.resourceRunRequested.modify(_.map {
      case (r, ResourceRunOperation.ResourceRunCompleted(_)) if re === r =>
        r -> ResourceRunOperation.ResourceRunIdle
      case r => r
    })

  val Default: TabOperations =
    TabOperations(
      RunOperation.RunIdle,
      SyncOperation.SyncIdle,
      PauseOperation.PauseIdle,
      CancelPauseOperation.CancelPauseIdle,
      ResumeOperation.ResumeIdle,
      StopOperation.StopIdle,
      AbortOperation.AbortIdle,
      StartFromOperation.StartFromIdle,
      SortedMap.empty
    )
}
