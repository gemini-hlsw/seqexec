// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.{Eq, Order}
import cats.implicits._
import cats.data.NonEmptyList
import diode._
import gem.Observation
import gem.enum.Site
import seqexec.model._
import seqexec.model.enum._
import seqexec.web.client.model._
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.QueueTableBody
import web.client.table._

package circuit {
  // All these classes are focused views of the root model. They are used to only update small sections of the
  // UI even if other parts of the root model change
  final case class WebSocketsFocus(location: Pages.SeqexecPages, sequences: SequencesQueue[SequenceView], user: Option[UserDetails], clientId: Option[ClientID], site: Option[Site]) extends UseValueEq

  final case class InitialSyncFocus(location: Pages.SeqexecPages, firstLoad: Boolean) extends UseValueEq

  final case class SequenceInQueue(id: Observation.Id, status: SequenceState, instrument: Instrument, active: Boolean, loaded: Boolean, name: String, targetName: Option[TargetName], runningStep: Option[RunningStep]) extends UseValueEq

  object SequenceInQueue {
    implicit val order: Order[SequenceInQueue] = Order.by(_.id)
    implicit val ordering: scala.math.Ordering[SequenceInQueue] = order.toOrdering
  }

  final case class StatusAndLoadedSequencesFocus(status: ClientStatus, sequences: List[SequenceInQueue], tableState: TableState[QueueTableBody.TableColumn]) extends UseValueEq

  final case class HeaderSideBarFocus(status: ClientStatus, conditions: Conditions, operator: Option[Operator]) extends UseValueEq

  final case class InstrumentStatusFocus(instrument: Instrument, active: Boolean, idState: Option[(Observation.Id, SequenceState)], runningStep: Option[RunningStep]) extends UseValueEq

  final case class InstrumentTabFocus(tabs: NonEmptyList[AvailableTab], user: Option[UserDetails]) extends UseValueEq

  final case class SequenceTabContentFocus(instrument: Option[Instrument], id: Option[Observation.Id], sequenceSelected: Boolean, logDisplayed: SectionVisibilityState) extends UseValueEq

  object SequenceTabContentFocus {
    implicit val eq: Eq[SequenceTabContentFocus] =
      Eq.by(x => (x.instrument, x.id, x.sequenceSelected, x.logDisplayed))
  }

  final case class StatusAndObserverFocus(isLogged: Boolean, obsName: Option[String], /*instrument: Option[Instrument],*/ id: Observation.Id, observer: Option[Observer], status: Option[SequenceState], targetName: Option[TargetName]) extends UseValueEq

  final case class StatusAndStepFocus(isLogged: Boolean, instrument: Instrument, obsId: Observation.Id, stepConfigDisplayed: Option[Int], totalSteps: Int, isPreview: Boolean) extends UseValueEq

  final case class StepsTableFocus(id: Observation.Id, instrument: Instrument, state: SequenceState, steps: List[Step], stepConfigDisplayed: Option[Int], nextStepToRun: Option[Int], isPreview: Boolean) extends UseValueEq

  final case class StepsTableAndStatusFocus(status: ClientStatus, stepsTable: Option[StepsTableFocus], configTableState: TableState[StepConfigTable.TableColumn]) extends UseValueEq

  final case class ControlModel(id: Observation.Id, isPartiallyExecuted: Boolean, nextStepToRun: Option[Int], status: SequenceState, inConflict: Boolean) extends UseValueEq

  final case class SequenceControlFocus(isLogged: Boolean, isConnected: Boolean, control: Option[ControlModel], syncInProgress: Boolean) extends UseValueEq

  final case class TableStates(queueTable: TableState[QueueTableBody.TableColumn], stepConfigTable: TableState[StepConfigTable.TableColumn]) extends UseValueEq
}

package object circuit {
}
