// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.model

import cats.Eq
import cats.implicits._
import monocle.macros.Lenses
import seqexec.model.Observer
import seqexec.model.UserDetails
import seqexec.web.common.FixedLengthBuffer
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.SessionQueueTableBody
import web.client.table._

/**
  * UI model, changes here will update the UI
  */
@Lenses
final case class SeqexecUIModel(
  navLocation:        Pages.SeqexecPages,
  user:               Option[UserDetails],
  loginBox:           SectionVisibilityState,
  globalLog:          GlobalLog,
  sequencesOnDisplay: SequencesOnDisplay,
  configTableState:   TableState[StepConfigTable.TableColumn],
  queueTableState:    TableState[SessionQueueTableBody.TableColumn],
  defaultObserver:    Observer,
  notification:       UserNotificationState,
  queues:             CalibrationQueues,
  obsProgress:        ObservationsProgress,
  firstLoad:          Boolean)

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SeqexecUIModel {
  val Initial: SeqexecUIModel = SeqexecUIModel(
    Pages.Root,
    None,
    SectionClosed,
    GlobalLog(FixedLengthBuffer.unsafeFromInt(500), SectionClosed),
    SequencesOnDisplay.Empty,
    StepConfigTable.InitialTableState,
    SessionQueueTableBody.InitialTableState.tableState,
    Observer(""),
    UserNotificationState.Empty,
    CalibrationQueues.Default,
    ObservationsProgress.Empty,
    firstLoad = true
  )

  implicit val eq: Eq[SeqexecUIModel] =
    Eq.by(
      x =>
        (x.navLocation,
         x.user,
         x.loginBox,
         x.globalLog,
         x.sequencesOnDisplay,
         x.configTableState,
         x.queueTableState,
         x.defaultObserver,
         x.notification,
         x.queues,
         x.firstLoad))

  val defaultObserverG = SeqexecUIModel.defaultObserver.asGetter
}
