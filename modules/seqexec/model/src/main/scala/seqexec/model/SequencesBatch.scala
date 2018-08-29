// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq

object SequencesBatch {
  sealed trait CommandState extends Product

  object CommandState {
    case object Idle extends CommandState
    case object Run extends CommandState
    case object Stop extends CommandState
  }
  implicit val cmdStateEq: Eq[CommandState] = Eq.fromUniversalEquals

  sealed trait State

  object State {
    case object Idle extends State // Queue is not running, and has unfinished sequences.
    case object Running extends State // Queue was commanded to run, and at least one sequence is running.
    case object Waiting extends State // Queue was commanded to run, but it is waiting for resources.
    case object Stopping extends State // Queue was commanded to stop, but at least one sequence is still running.
    case object Completed extends State // All sequences in the queue were run to completion.

    implicit val equal: Eq[State] = Eq.fromUniversalEquals
  }
}
