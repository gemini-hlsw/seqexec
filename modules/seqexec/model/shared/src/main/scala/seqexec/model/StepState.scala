// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._

sealed abstract class StepState(val canRunFrom: Boolean)
  extends Product with Serializable

object StepState {

        case object Pending             extends StepState(true)
        case object Completed           extends StepState(false)
        case object Skipped             extends StepState(false)
        case object Aborted             extends StepState(true)
  final case class  Failed(msg: String) extends StepState(true)
        case object Running             extends StepState(false)
        case object Paused              extends StepState(true)

  implicit val equal: Eq[StepState] =
    Eq.instance {
      case (Pending, Pending)     => true
      case (Completed, Completed) => true
      case (Skipped, Skipped)     => true
      case (Failed(a), Failed(b)) => a === b
      case (Running, Running)     => true
      case (Paused, Paused)       => true
      case (Aborted, Aborted)     => true
      case _                      => false
    }

    implicit class StepStateOps(val s: StepState) extends AnyVal {
      def canSetBreakpoint(i: Int, firstRunnable: Int): Boolean = s match {
        case StepState.Pending | StepState.Skipped | StepState.Paused |
            StepState.Running | StepState.Aborted => i > firstRunnable
        case _ => false
      }

      def canSetSkipmark: Boolean = s match {
        case StepState.Pending | StepState.Paused | StepState.Aborted => true
        case _ if hasError                                            => true
        case _                                                        => false
      }

      def hasError: Boolean = s match {
        case StepState.Failed(_) => true
        case _                   => false
      }

      def isRunning: Boolean = s === StepState.Running

      def isPending: Boolean = s === StepState.Pending

      def runningOrComplete: Boolean = s match {
        case StepState.Running | StepState.Completed => true
        case _                                       => false
      }

      def isFinished: Boolean = s match {
        case StepState.Completed | StepState.Skipped => true
        case _                                       => false
      }

      def wasSkipped: Boolean = s === StepState.Skipped

      def canConfigure: Boolean = s match {
        case StepState.Pending | StepState.Paused | StepState.Failed(_) | StepState.Aborted => true
        case _                                                                              => false
      }

    }
}
