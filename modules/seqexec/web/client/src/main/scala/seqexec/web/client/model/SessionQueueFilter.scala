// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses
import seqexec.web.client.circuit.SequenceInSessionQueue

sealed trait ObsClass

object ObsClass {
  case object All extends ObsClass
  case object Daytime extends ObsClass
  case object Nighttime extends ObsClass

  implicit val eq: Eq[ObsClass] =
    Eq.fromUniversalEquals

  def fromString(s: String): ObsClass = s match {
    case "dayCal" => Daytime
    case _        => Nighttime
  }
}

/**
  * Model of a filter for the session queue
  */
@Lenses
final case class SessionQueueFilter(obsClass: ObsClass) {
  val dayTimeSelected: Boolean = obsClass match {
    case ObsClass.All | ObsClass.Daytime => true
    case _                               => false
  }

  val nightTimeSelected: Boolean = obsClass match {
    case ObsClass.All | ObsClass.Nighttime => true
    case _                                 => false
  }

  def filter(seq: List[SequenceInSessionQueue]): List[SequenceInSessionQueue] =
    obsClass match {
      case ObsClass.All       => seq
      case ObsClass.Daytime   => seq.filter(_.obsClass === ObsClass.Daytime)
      case ObsClass.Nighttime => seq.filter(_.obsClass === ObsClass.Nighttime)
    }
}

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SessionQueueFilter {
  implicit val eq: Eq[SessionQueueFilter] =
    Eq.by(_.obsClass)

  val NoFilter: SessionQueueFilter = SessionQueueFilter(ObsClass.All)
}
