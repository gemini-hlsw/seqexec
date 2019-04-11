// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.Lens
import monocle.macros.Lenses
import monocle.function.At.at
import monocle.function.At.atSortedMap
import seqexec.model.enum.Resource
import scala.collection.immutable.SortedMap

sealed trait RunOperation extends Product with Serializable
object RunOperation {
  case object RunInFlight extends RunOperation
  case object RunIdle extends RunOperation

  implicit val eq: Eq[RunOperation] =
    Eq.fromUniversalEquals

}

sealed trait StopOperation extends Product with Serializable
object StopOperation {
  case object StopInFlight extends StopOperation
  case object StopIdle extends StopOperation

  implicit val eq: Eq[StopOperation] =
    Eq.fromUniversalEquals

}

sealed trait AbortOperation extends Product with Serializable
object AbortOperation {
  case object AbortInFlight extends AbortOperation
  case object AbortIdle extends AbortOperation

  implicit val eq: Eq[AbortOperation] =
    Eq.fromUniversalEquals

}

sealed trait PauseOperation extends Product with Serializable
object PauseOperation {
  case object PauseInFlight extends PauseOperation
  case object PauseIdle extends PauseOperation

  implicit val eq: Eq[PauseOperation] =
    Eq.fromUniversalEquals

}

sealed trait ResumeOperation extends Product with Serializable
object ResumeOperation {
  case object ResumeInFlight extends ResumeOperation
  case object ResumeIdle extends ResumeOperation

  implicit val eq: Eq[ResumeOperation] =
    Eq.fromUniversalEquals

}

sealed trait SyncOperation extends Product with Serializable
object SyncOperation {
  case object SyncInFlight extends SyncOperation
  case object SyncIdle extends SyncOperation

  implicit val eq: Eq[SyncOperation] =
    Eq.fromUniversalEquals

}

sealed trait ResourceRunOperation extends Product with Serializable
object ResourceRunOperation {
  case object ResourceRunInFlight extends ResourceRunOperation
  case object ResourceRunIdle extends ResourceRunOperation

  implicit val eq: Eq[ResourceRunOperation] =
    Eq.fromUniversalEquals

}

sealed trait StartFromOperation extends Product with Serializable
object StartFromOperation {
  case object StartFromInFlight extends StartFromOperation
  case object StartFromIdle extends StartFromOperation

  implicit val eq: Eq[StartFromOperation] =
    Eq.fromUniversalEquals

}

/**
  * Hold transient states while excuting an operation
  */
@Lenses
final case class TabOperations(
  runRequested:         RunOperation,
  syncRequested:        SyncOperation,
  pauseRequested:       PauseOperation,
  resumeRequested:      ResumeOperation,
  stopRequested:        StopOperation,
  abortRequested:       AbortOperation,
  startFromRequested:   StartFromOperation,
  resourceRunRequested: SortedMap[Resource, ResourceRunOperation]) {
  // Indicate if any resource is being executed
  def resourceInFlight: Boolean =
    resourceRunRequested.exists(
      _._2 === ResourceRunOperation.ResourceRunInFlight)

  val stepRequestInFlight: Boolean =
    pauseRequested === PauseOperation.PauseInFlight ||
      resumeRequested === ResumeOperation.ResumeInFlight ||
      stopRequested === StopOperation.StopInFlight ||
      abortRequested === AbortOperation.AbortInFlight ||
      startFromRequested === StartFromOperation.StartFromInFlight
}

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object TabOperations {
  implicit val eq: Eq[TabOperations] =
    Eq.by(
      x =>
        (x.runRequested,
         x.syncRequested,
         x.pauseRequested,
         x.resumeRequested,
         x.stopRequested,
         x.abortRequested,
         x.startFromRequested,
         x.resourceRunRequested))

  def resourceRun(
    r: Resource): Lens[TabOperations, Option[ResourceRunOperation]] =
    TabOperations.resourceRunRequested ^|-> at(r)

  val Default: TabOperations =
    TabOperations(
      RunOperation.RunIdle,
      SyncOperation.SyncIdle,
      PauseOperation.PauseIdle,
      ResumeOperation.ResumeIdle,
      StopOperation.StopIdle,
      AbortOperation.AbortIdle,
      StartFromOperation.StartFromIdle,
      SortedMap.empty
    )
}
