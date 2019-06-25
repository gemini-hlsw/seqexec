// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import gem.util.Enumerated
import monocle.macros.Lenses

sealed trait AddDayCalOperation extends Product with Serializable
object AddDayCalOperation {
  case object AddDayCalIdle extends AddDayCalOperation
  case object AddDayCalInFlight extends AddDayCalOperation

  implicit val AddDayCalOperationEnumerated: Enumerated[AddDayCalOperation] =
    Enumerated.of(AddDayCalIdle, AddDayCalInFlight)

}

sealed trait ClearAllCalOperation extends Product with Serializable
object ClearAllCalOperation {
  case object ClearAllCalIdle extends ClearAllCalOperation
  case object ClearAllCalInFlight extends ClearAllCalOperation

  implicit val ClearAllCalOperationEnumerated: Enumerated[ClearAllCalOperation] =
    Enumerated.of(ClearAllCalIdle, ClearAllCalInFlight)

}

sealed trait RunCalOperation extends Product with Serializable
object RunCalOperation {
  case object RunCalIdle extends RunCalOperation
  case object RunCalInFlight extends RunCalOperation

  implicit val RunCalOperationEnumerated: Enumerated[RunCalOperation] =
    Enumerated.of(RunCalIdle, RunCalInFlight)

}

sealed trait StopCalOperation extends Product with Serializable
object StopCalOperation {
  case object StopCalIdle extends StopCalOperation
  case object StopCalInFlight extends StopCalOperation

  implicit val StopCalOperationEnumerated: Enumerated[StopCalOperation] =
    Enumerated.of(StopCalIdle, StopCalInFlight)

}

/**
  * Hold transient states while excuting an operation on the queue
  */
@Lenses
final case class QueueOperations(addDayCalRequested:   AddDayCalOperation,
                                 clearAllCalRequested: ClearAllCalOperation,
                                 runCalRequested:      RunCalOperation,
                                 stopCalRequested:     StopCalOperation)

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
