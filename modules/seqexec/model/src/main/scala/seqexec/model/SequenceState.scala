// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._

sealed trait SequenceState extends Product with Serializable {
  import SequenceState._

  def internalStopRequested: Boolean =
    this match {
      case SequenceState.Running(_, b) => b
      case _                           => false
    }

  def isError: Boolean =
    this match {
      case Failed(_) => true
      case _         => false
    }

  def isInProcess: Boolean =
    this =!= SequenceState.Idle

  def isRunning: Boolean =
    this match {
      case Running(_, _) => true
      case _             => false
    }

  def isCompleted: Boolean =
    this === SequenceState.Completed

  def isIdle: Boolean =
    this === SequenceState.Idle

  def isStopped: Boolean =
    this === SequenceState.Stopped

  def userStopRequested: Boolean =
    this match {
      case SequenceState.Running(b, _) => b
      case _                           => false
    }

}

object SequenceState {

        case object Completed                                         extends SequenceState
        case object Idle                                              extends SequenceState
        case object Stopped                                           extends SequenceState
  final case class  Running(userStop: Boolean, internalStop: Boolean) extends SequenceState
  final case class  Failed(msg: String)                               extends SequenceState

  object Running {
    val init: Running =
      Running(userStop = false, internalStop = false)
  }

  implicit val equal: Eq[SequenceState] =
    Eq.fromUniversalEquals

}
