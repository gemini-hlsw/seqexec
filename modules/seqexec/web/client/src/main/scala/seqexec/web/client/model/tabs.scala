// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.Observation
import monocle.Lens
import monocle.Prism
import monocle.macros.GenPrism
import monocle.macros.Lenses
import seqexec.model.Observer
import seqexec.model.SequenceState
import seqexec.model.SequenceView
import seqexec.model.enum._
import seqexec.web.client.ModelOps._
import seqexec.web.client.components.sequence.steps.StepsTable
import web.client.table._

final case class AvailableTab(id:            Option[Observation.Id],
                              status:        Option[SequenceState],
                              instrument:    Option[Instrument],
                              runningStep:   Option[RunningStep],
                              nextStepToRun: Option[Int],
                              isPreview:     Boolean,
                              active:        TabSelected,
                              loading:       Boolean) {
  val nonEmpty: Boolean = id.isDefined
}

object AvailableTab {
  implicit val eq: Eq[AvailableTab] =
    Eq.by(
      x =>
        (x.id,
         x.status,
         x.instrument,
         x.runningStep,
         x.nextStepToRun,
         x.isPreview,
         x.active,
         x.loading))
}

final case class CalibrationQueueTabActive(calibrationTab: CalibrationQueueTab,
                                           active:         TabSelected)

object CalibrationQueueTabActive {
  implicit val eq: Eq[CalibrationQueueTabActive] =
    Eq.by(x => (x.calibrationTab, x.active))
}

sealed trait TabSelected extends Product with Serializable
object TabSelected {
  case object Selected extends TabSelected
  case object Background extends TabSelected

  implicit val eq: Eq[TabSelected] =
    Eq.fromUniversalEquals

  def fromBoolean(b: Boolean): TabSelected = if (b) Selected else Background

}

final case class SeqexecTabActive(tab: SequenceTab, active: TabSelected)

object SeqexecTabActive {
  implicit val eq: Eq[SeqexecTabActive] =
    Eq.by(x => (x.tab, x.active))

}

sealed trait SeqexecTab {
  type TC
  val tableState: TableState[TC]

  def isPreview: Boolean
}

object SeqexecTab {
  implicit val eq: Eq[SeqexecTab] =
    Eq.instance {
      case (a: SequenceTab, b: SequenceTab)                 => a === b
      case (a: CalibrationQueueTab, b: CalibrationQueueTab) => a === b
      case _                                                => false
    }

  val previewTab: Prism[SeqexecTab, PreviewSequenceTab] =
    GenPrism[SeqexecTab, PreviewSequenceTab]
  val instrumentTab: Prism[SeqexecTab, InstrumentSequenceTab] =
    GenPrism[SeqexecTab, InstrumentSequenceTab]
  val calibrationTab: Prism[SeqexecTab, CalibrationQueueTab] =
    GenPrism[SeqexecTab, CalibrationQueueTab]
  val sequenceTab: Prism[SeqexecTab, SequenceTab] =
    Prism.partial[SeqexecTab, SequenceTab] {
      case p: PreviewSequenceTab    => p
      case i: InstrumentSequenceTab => i
    }(identity)
}

@Lenses
final case class CalibrationQueueTab(
  tableState: TableState[StepsTable.TableColumn],
  state:      BatchExecState,
  observer:   Option[Observer])
    extends SeqexecTab {
  type TC = StepsTable.TableColumn
  val isPreview: Boolean = false
}

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object CalibrationQueueTab {
  val Empty: CalibrationQueueTab =
    CalibrationQueueTab(StepsTable.State.InitialTableState,
                        BatchExecState.Idle,
                        None)

  implicit val eq: Eq[CalibrationQueueTab] =
    Eq.by(x => (x.tableState, x.state, x.observer))
}

sealed trait SequenceTab extends SeqexecTab {
  type TC = StepsTable.TableColumn
  val tabOperations: TabOperations

  def instrument: Option[Instrument] = this match {
    case i: InstrumentSequenceTab => i.inst.some
    case i: PreviewSequenceTab    => i.currentSequence.metadata.instrument.some
  }

  def sequence: Option[SequenceView] = this match {
    // Returns the current sequence or if empty the last completed one
    case i: InstrumentSequenceTab => i.currentSequence.orElse(i.completedSequence)
    case i: PreviewSequenceTab    => i.currentSequence.some
  }

  def obsId: Option[Observation.Id] = sequence.map(_.id)

  def stepConfigDisplayed: Option[Int] = this match {
    case i: InstrumentSequenceTab => i.stepConfig
    case i: PreviewSequenceTab    => i.stepConfig
  }

  def isPreview: Boolean = this match {
    case _: InstrumentSequenceTab => false
    case _                        => true
  }

  def runningStep: Option[RunningStep] = this match {
    case _: InstrumentSequenceTab => sequence.flatMap(_.runningStep)
    case _                        => none
  }

  def nextStepToRun: Option[Int] = sequence.foldMap(_.nextStepToRun)

  def loading: Boolean = this match {
    case _: InstrumentSequenceTab => false
    case p: PreviewSequenceTab    => p.isLoading
  }
}

object SequenceTab {
  implicit val eq: Eq[SequenceTab] =
    Eq.instance {
      case (a: InstrumentSequenceTab, b: InstrumentSequenceTab) => a === b
      case (a: PreviewSequenceTab, b: PreviewSequenceTab)       => a === b
      case _                                                    => false
    }

  // Some lenses
  val stepConfigL: Lens[SequenceTab, Option[Int]] =
    Lens[SequenceTab, Option[Int]] {
      case t: InstrumentSequenceTab => t.stepConfig
      case t: PreviewSequenceTab    => t.stepConfig
    }(n =>
      a =>
        a match {
          case t: InstrumentSequenceTab => t.copy(stepConfig = n)
          case t: PreviewSequenceTab    => t.copy(stepConfig = n)
    })

}

@Lenses
final case class InstrumentSequenceTab(
  inst:              Instrument,
  currentSequence:   Option[SequenceView],
  completedSequence: Option[SequenceView],
  stepConfig:        Option[Int],
  tableState:        TableState[StepsTable.TableColumn],
  tabOperations:     TabOperations)
    extends SequenceTab

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object InstrumentSequenceTab {
  implicit val eq: Eq[InstrumentSequenceTab] =
    Eq.by(
      x =>
        (x.instrument,
         x.currentSequence,
         x.completedSequence,
         x.stepConfig,
         x.tableState,
         x.tabOperations))
}

@Lenses
final case class PreviewSequenceTab(
  currentSequence: SequenceView,
  stepConfig:      Option[Int],
  isLoading:       Boolean,
  tableState:      TableState[StepsTable.TableColumn],
  tabOperations:   TabOperations)
    extends SequenceTab

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object PreviewSequenceTab {
  implicit val eq: Eq[PreviewSequenceTab] =
    Eq.by(
      x =>
        (x.currentSequence,
         x.stepConfig,
         x.isLoading,
         x.tableState,
         x.tabOperations))
}
