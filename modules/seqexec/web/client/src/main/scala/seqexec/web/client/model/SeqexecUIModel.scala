// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.{ Conditions, Observer, UserDetails, SequencesQueue, SequenceView }
import seqexec.web.common.FixedLengthBuffer
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.QueueTableBody
import web.client.table._

/**
 * UI model, changes here will update the UI
 */
@Lenses
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
                          defaultObserver: Observer,
                          firstLoad: Boolean)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
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
    Observer(""),
    firstLoad = true)

  implicit val eq: Eq[SeqexecUIModel] =
    Eq.by(x => (x.navLocation, x.user, x.sequences, x.loginBox, x.resourceConflict, x.globalLog, x.sequencesOnDisplay, x.configTableState, x.queueTableState, x.defaultObserver, x.firstLoad))
}
