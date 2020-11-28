// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.syntax.all._
import lucuma.core.util.Enumerated
import monocle.macros.Lenses
import seqexec.model.SequenceView
import seqexec.web.client.circuit.SequenceInSessionQueue
import seqexec.web.client.model.lenses.obsClassT

sealed trait ObsClass extends Product with Serializable

object ObsClass {
  case object All extends ObsClass
  case object Daytime extends ObsClass
  case object Nighttime extends ObsClass

  /** @group Typeclass Instances */
  implicit val ObsClassEnumerated: Enumerated[ObsClass] =
    Enumerated.of(All, Daytime, Nighttime)

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
    case ObsClass.Daytime => true
    case _                => false
  }

  val nightTimeSelected: Boolean = obsClass match {
    case ObsClass.Nighttime => true
    case _                  => false
  }

  def filter(seq: List[SequenceInSessionQueue]): List[SequenceInSessionQueue] =
    obsClass match {
      case ObsClass.All       => seq
      case ObsClass.Daytime   => seq.filter(_.obsClass === ObsClass.Daytime)
      case ObsClass.Nighttime => seq.filter(_.obsClass === ObsClass.Nighttime)
    }

  def filterS(seq: List[SequenceView]): List[SequenceView] =
    obsClass match {
      case ObsClass.All => seq
      case ObsClass.Daytime =>
        seq.filter(
          obsClassT
            .headOption(_)
            .map(ObsClass.fromString) === ObsClass.Daytime.some)
      case ObsClass.Nighttime =>
        seq.filter(
          obsClassT
            .headOption(_)
            .map(ObsClass.fromString) === ObsClass.Nighttime.some)
    }

  def isFilterApplied: Boolean = obsClass =!= ObsClass.All
}

object SessionQueueFilter {
  implicit val eq: Eq[SessionQueueFilter] =
    Eq.by(_.obsClass)

  val NoFilter: SessionQueueFilter = SessionQueueFilter(ObsClass.All)
}
