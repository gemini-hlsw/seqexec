// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq
import cats.Show

sealed trait BatchExecState extends Product with Serializable {
  def running: Boolean = this match {
    case BatchExecState.Running |
         BatchExecState.Waiting => true
    case _                      => false
  }
}

object BatchExecState {
  case object Idle extends BatchExecState // Queue is not running, and has unfinished sequences.
  case object Running extends BatchExecState // Queue was commanded to run, and at least one sequence is running.
  case object Waiting extends BatchExecState // Queue was commanded to run, but it is waiting for resources.
  case object Stopping extends BatchExecState // Queue was commanded to stop, but at least one sequence is still running.
  case object Completed extends BatchExecState // All sequences in the queue were run to completion.

  implicit val equal: Eq[BatchExecState] = Eq.fromUniversalEquals

  implicit val show: Show[BatchExecState] = Show.show {
    case Idle      => "Idle"
    case Running   => "Running"
    case Waiting   => "Waiting"
    case Stopping  => "Stopping"
    case Completed => "Completed"

  }
}
