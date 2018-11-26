// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
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

sealed trait SyncOperation extends Product with Serializable
object SyncOperation {
  case object SyncInFlight extends SyncOperation
  case object SyncIdle extends SyncOperation

  implicit val eq: Eq[SyncOperation] =
    Eq.fromUniversalEquals

}

sealed trait PauseOperation extends Product with Serializable
object PauseOperation {
  case object PauseInFlight extends PauseOperation
  case object PauseIdle extends PauseOperation

  implicit val eq: Eq[PauseOperation] =
    Eq.fromUniversalEquals

}

sealed trait ResourceRunOperation extends Product with Serializable
object ResourceRunOperation {
  case object ResourceRunInFlight extends ResourceRunOperation
  case object ResourceRunIdle extends ResourceRunOperation

  implicit val eq: Eq[ResourceRunOperation] =
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
  resourceRunRequested: SortedMap[Resource, ResourceRunOperation]) {
  // Indicate if any resource is being executed
  def resourceInFlight: Boolean =
    resourceRunRequested.exists(
      _._2 === ResourceRunOperation.ResourceRunInFlight)
}

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object TabOperations {
  implicit val eq: Eq[TabOperations] =
    Eq.by(
      x =>
        (x.runRequested,
         x.syncRequested,
         x.pauseRequested,
         x.resourceRunRequested))

  def resourceRun(
    r: Resource): Lens[TabOperations, Option[ResourceRunOperation]] =
    TabOperations.resourceRunRequested ^|-> at(r)

  val Default: TabOperations =
    TabOperations(RunOperation.RunIdle,
                  SyncOperation.SyncIdle,
                  PauseOperation.PauseIdle,
                  SortedMap.empty)
}
