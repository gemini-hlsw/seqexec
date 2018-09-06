// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import gem.Observation
import monocle.{Lens, Optional}
import monocle.macros.Lenses
import seqexec.model.{ SequenceState, SequenceView }
import seqexec.model.enum._
import seqexec.web.client.ModelOps._
import seqexec.web.client.components.sequence.steps.StepsTable
import web.client.table._

final case class AvailableTab(id: Option[Observation.Id], status: Option[SequenceState], instrument: Option[Instrument], runningStep: Option[RunningStep], nextStepToRun: Option[Int], isPreview: Boolean, active: Boolean, loading: Boolean) {
  val nonEmpty: Boolean = id.isDefined
}

object AvailableTab {
  implicit val eq: Eq[AvailableTab] =
    Eq.by(x => (x.id, x.status, x.instrument, x.runningStep, x.nextStepToRun, x.isPreview, x.active, x.loading))
}

final case class SequenceTabActive(tab: SequenceTab, active: Boolean)

object SequenceTabActive {
  implicit val eq: Eq[SequenceTabActive] =
    Eq.by(x => (x.tab, x.active))

  val Empty: SequenceTabActive = SequenceTabActive(SequenceTab.Empty, true)
}

sealed trait SequenceTab {
  val tableState: TableState[StepsTable.TableColumn]

  def instrument: Option[Instrument] = this match {
    case i: InstrumentSequenceTab => i.inst.some
    case i: PreviewSequenceTab    => i.currentSequence.map(_.metadata.instrument)
  }

  def sequence: Option[SequenceView] = this match {
    // Returns the current sequence or if empty the last completed one
    case i: InstrumentSequenceTab => i.currentSequence.orElse(i.completedSequence)
    case i: PreviewSequenceTab    => i.currentSequence
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

@Lenses
final case class InstrumentSequenceTab(inst: Instrument, currentSequence: Option[SequenceView], completedSequence: Option[SequenceView], stepConfig: Option[Int], tableState: TableState[StepsTable.TableColumn]) extends SequenceTab

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object InstrumentSequenceTab {
  implicit val eq: Eq[InstrumentSequenceTab] =
    Eq.by(x => (x.instrument, x.currentSequence, x.completedSequence, x.stepConfig, x.tableState))
}

@Lenses
final case class PreviewSequenceTab(currentSequence: Option[SequenceView], stepConfig: Option[Int], isLoading: Boolean, tableState: TableState[StepsTable.TableColumn]) extends SequenceTab

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object PreviewSequenceTab {
  implicit val eq: Eq[PreviewSequenceTab] =
    Eq.by(x => (x.currentSequence, x.stepConfig, x.isLoading, x.tableState))
}

object SequenceTab {
  implicit val eq: Eq[SequenceTab] =
    Eq.instance {
      case (a: InstrumentSequenceTab, b: InstrumentSequenceTab) => a === b
      case (a: PreviewSequenceTab, b: PreviewSequenceTab)       => a === b
      case _                                                    => false
    }
  val Empty: SequenceTab = PreviewSequenceTab(None, None, false, StepsTable.State.InitialTableState)

  // Some lenses
  val stepConfigL: Lens[SequenceTab, Option[Int]] = Lens[SequenceTab, Option[Int]] {
    case t: InstrumentSequenceTab => t.stepConfig
    case t: PreviewSequenceTab    => t.stepConfig
  }(n => a => a match {
    case t: InstrumentSequenceTab => t.copy(stepConfig = n)
    case t: PreviewSequenceTab    => t.copy(stepConfig = n)
  })

  val currentSequenceL: Lens[SequenceTab, Option[SequenceView]] = Lens[SequenceTab, Option[SequenceView]] {
    case t: InstrumentSequenceTab => t.currentSequence
    case t: PreviewSequenceTab    => t.currentSequence
  }(n => a => a match {
    case t: InstrumentSequenceTab => t.copy(currentSequence = n)
    case t: PreviewSequenceTab    => t.copy(currentSequence = n)
  })

  val completedSequenceO: Optional[SequenceTab, Option[SequenceView]] = Optional[SequenceTab, Option[SequenceView]] {
    case t: InstrumentSequenceTab => t.completedSequence.some
    case _: PreviewSequenceTab    => None
  }(n => a => a match {
    case t: InstrumentSequenceTab => t.copy(completedSequence = n)
    case t: PreviewSequenceTab    => t
  })
}
