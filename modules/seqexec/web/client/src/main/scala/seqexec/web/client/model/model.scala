// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats._
import cats.implicits._
import diode.data.Pot
import gem.Observation
import gem.enum.Site
import seqexec.model.{ ClientID, Conditions, UserDetails, SequencesQueue, SequenceView, StepId }
import seqexec.model.enum._
import seqexec.model.events._
import seqexec.web.common.FixedLengthBuffer
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.QueueTableBody
import org.scalajs.dom.WebSocket
import web.client.table._

// Pages
object Pages {
  sealed trait SeqexecPages extends Product with Serializable

  case object Root extends SeqexecPages
  case object SoundTest extends SeqexecPages
  final case class InstrumentPage(instrument: Instrument) extends SeqexecPages
  final case class PreviewPage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages
  final case class PreviewConfigPage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages
  final case class SequencePage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages
  final case class SequenceConfigPage(instrument: Instrument, obsId: Observation.Id, step: StepId) extends SeqexecPages

  implicit val equal: Eq[SeqexecPages] = Eq.instance {
    case (Root, Root)                                               => true
    case (SoundTest, SoundTest)                                     => true
    case (InstrumentPage(i), InstrumentPage(j))                     => i === j
    case (SequencePage(i, o, s), SequencePage(j, p, r))             => i === j && o === p && s === r
    case (SequenceConfigPage(i, o, s), SequenceConfigPage(j, p, r)) => i === j && o === p && s === r
    case (PreviewPage(i, o, s), PreviewPage(j, p, r))               => i === j && o === p && s === r
    case (PreviewConfigPage(i, o, s), PreviewConfigPage(j, p, r))   => i === j && o === p && s === r
    case _                                                          => false
  }
}

final case class RunningStep(last: Int, total: Int)

object RunningStep {
  implicit val show: Show[RunningStep] =
    Show.show(u => s"${u.last + 1}/${u.total}")

  implicit val eq: Eq[RunningStep] =
    Eq.by(x => (x.last, x.total))
}

// UI model
sealed trait SectionVisibilityState extends Product with Serializable
case object SectionOpen extends SectionVisibilityState
case object SectionClosed extends SectionVisibilityState

object SectionVisibilityState {
  implicit val eq: Eq[SectionVisibilityState] = Eq.fromUniversalEquals
}

final case class WebSocketConnection(ws: Pot[WebSocket], nextAttempt: Int, autoReconnect: Boolean)

object WebSocketConnection {
  val Empty: WebSocketConnection = WebSocketConnection(diode.data.Empty, 0, autoReconnect = true)

  implicit val equal: Eq[WebSocketConnection] =
    Eq.by { x =>
      (x.ws, x.nextAttempt, x.autoReconnect)
    }

}

/**
  * Keeps a list of log entries for display
  */
final case class GlobalLog(log: FixedLengthBuffer[ServerLogMessage], display: SectionVisibilityState)

/**
 * Model to display a resource conflict
 */
final case class ResourcesConflict(visibility: SectionVisibilityState, id: Option[Observation.Id])

/**
 * UI model, changes here will update the UI
 */
final case class SeqexecUIModel(navLocation: Pages.SeqexecPages,
                          user: Option[UserDetails],
                          sequences: SequencesQueue[SequenceView],
                          loginBox: SectionVisibilityState,
                          resourceConflict: ResourcesConflict,
                          globalLog: GlobalLog,
                          sequencesOnDisplay: SequencesOnDisplay,
                          syncInProgress: Boolean,
                          configTableState: TableState[StepConfigTable.TableColumn],
                          queueTableState: TableState[QueueTableBody.TableColumn],
                          firstLoad: Boolean)

object SeqexecUIModel {
  val noSequencesLoaded: SequencesQueue[SequenceView] = SequencesQueue[SequenceView](Map.empty, Conditions.Default, None, Nil)
  val Initial: SeqexecUIModel = SeqexecUIModel(
    Pages.Root,
    None,
    noSequencesLoaded,
    SectionClosed,
    ResourcesConflict(SectionClosed, None),
    GlobalLog(FixedLengthBuffer.unsafeFromInt(500), SectionClosed),
    SequencesOnDisplay.empty,
    syncInProgress = false,
    StepConfigTable.InitialTableState,
    QueueTableBody.InitialTableState.tableState,
    firstLoad = true)
}

/**
  * Root of the UI Model of the application
  */
final case class SeqexecAppRootModel(ws: WebSocketConnection, site: Option[Site], clientId: Option[ClientID], uiModel: SeqexecUIModel)

object SeqexecAppRootModel {
  val Initial: SeqexecAppRootModel = SeqexecAppRootModel(WebSocketConnection.Empty, None, None, SeqexecUIModel.Initial)
}
