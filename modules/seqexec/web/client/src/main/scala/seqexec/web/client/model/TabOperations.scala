// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses

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

/**
  * Hold transient states while excuting an operation
  */
@Lenses
final case class TabOperations(runRequested:  RunOperation,
                               syncRequested: SyncOperation)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object TabOperations {
  implicit val eq: Eq[TabOperations] =
    Eq.by(x => (x.runRequested, x.syncRequested))

  val Default: TabOperations =
    TabOperations(RunOperation.RunIdle, SyncOperation.SyncIdle)
}
