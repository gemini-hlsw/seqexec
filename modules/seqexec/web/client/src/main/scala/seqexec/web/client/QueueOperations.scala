// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses

sealed trait AddDayCalOperation extends Product with Serializable
object AddDayCalOperation {
  case object AddDayCalInFlight extends AddDayCalOperation
  case object AddDayCalIdle extends AddDayCalOperation

  implicit val eq: Eq[AddDayCalOperation] =
    Eq.fromUniversalEquals

}

sealed trait ClearAllCalOperation extends Product with Serializable
object ClearAllCalOperation {
  case object ClearAllCalInFlight extends ClearAllCalOperation
  case object ClearAllCalIdle extends ClearAllCalOperation

  implicit val eq: Eq[ClearAllCalOperation] =
    Eq.fromUniversalEquals

}

sealed trait RunCalOperation extends Product with Serializable
object RunCalOperation {
  case object RunCalInFlight extends RunCalOperation
  case object RunCalIdle extends RunCalOperation

  implicit val eq: Eq[RunCalOperation] =
    Eq.fromUniversalEquals

}

sealed trait StopCalOperation extends Product with Serializable
object StopCalOperation {
  case object StopCalInFlight extends StopCalOperation
  case object StopCalIdle extends StopCalOperation

  implicit val eq: Eq[StopCalOperation] =
    Eq.fromUniversalEquals

}

/**
  * Hold transient states while excuting an operation on the queue
  */
@Lenses
final case class QueueOperations(addDayCalRequested:   AddDayCalOperation,
                                 clearAllCalRequested: ClearAllCalOperation,
                                 runCalRequested:      RunCalOperation,
                                 stopCalRequested:     StopCalOperation)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object QueueOperations {
  implicit val eq: Eq[QueueOperations] =
    Eq.by(
      x =>
        (x.addDayCalRequested,
         x.clearAllCalRequested,
         x.runCalRequested,
         x.stopCalRequested))

  val Default: QueueOperations =
    QueueOperations(AddDayCalOperation.AddDayCalIdle,
                    ClearAllCalOperation.ClearAllCalIdle,
                    RunCalOperation.RunCalIdle,
                    StopCalOperation.StopCalIdle)
}
