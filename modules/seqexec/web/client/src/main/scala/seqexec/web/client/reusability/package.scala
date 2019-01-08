// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import diode.data.PotState
import cats.implicits._
import gem.Observation
import gem.enum.Site
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.extra.Reusability
import scala.collection.immutable.SortedMap
import seqexec.model.enum.Instrument
import seqexec.model.enum.BatchExecState
import seqexec.model.enum.Resource
import seqexec.model.Observer
import seqexec.model.QueueId
import seqexec.model.Step
import seqexec.model.StepConfig
import seqexec.model.StepState
import seqexec.model.UserDetails
import seqexec.model.SequenceState
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.model.SectionVisibilityState
import seqexec.web.client.model.UserNotificationState
import seqexec.web.client.model.WebSocketConnection
import seqexec.web.client.model.AddDayCalOperation
import seqexec.web.client.model.PauseOperation
import seqexec.web.client.model.QueueOperations
import seqexec.web.client.model.RunOperation
import seqexec.web.client.model.SyncOperation
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.model.TabSelected
import seqexec.web.client.model.SoundSelection
import seqexec.web.client.circuit._

package object reusability {
  implicit val stepStateReuse: Reusability[StepState]       = Reusability.byEq
  implicit val instrumentReuse: Reusability[Instrument]     = Reusability.byEq
  implicit val resourceReuse: Reusability[Resource]         = Reusability.byEq
  implicit val obsIdReuse: Reusability[Observation.Id]      = Reusability.byEq
  implicit val siteReuse: Reusability[Site]                 = Reusability.byEq
  implicit val observerReuse: Reusability[Observer]         = Reusability.byEq
  implicit val stepConfigReuse: Reusability[StepConfig]     = Reusability.byEq
  implicit val stepReuse: Reusability[Step]                 = Reusability.byEq
  implicit val seqStateReuse: Reusability[SequenceState]    = Reusability.byEq
  implicit val clientStatusReuse: Reusability[ClientStatus] = Reusability.byEq
  implicit val stepTTReuse: Reusability[StepsTableTypeSelection] = Reusability.byEq
  implicit val stTbFocusReuse: Reusability[StepsTableFocus] =
    Reusability.by { x =>
      (x.id,
       x.instrument,
       x.state,
       x.steps,
       x.stepConfigDisplayed,
       x.nextStepToRun,
       x.selectedStep,
       x.isPreview,
       x.tableState) // Don't include tabOperations on the check
    }
  implicit val stASFocusReuse: Reusability[StatusAndStepFocus] =
    Reusability.byEq
  implicit val sCFocusReuse: Reusability[SequenceControlFocus] =
    Reusability.byEq
  implicit val stfReuse: Reusability[StepsTableAndStatusFocus] =
    Reusability.byEq
  implicit val tabSelReuse: Reusability[TabSelected] = Reusability.byRef
  implicit val sectReuse: Reusability[SectionVisibilityState] =
    Reusability.byRef
  implicit val potStateReuse: Reusability[PotState] = Reusability.byRef
  implicit val webSCeuse: Reusability[WebSocketConnection] =
    Reusability.by(_.ws.state)
  implicit val runOperationReuse: Reusability[RunOperation] = Reusability.byRef
  implicit val syncOperationReuse: Reusability[SyncOperation] =
    Reusability.byRef
  implicit val psOperationReuse: Reusability[PauseOperation] = Reusability.byRef
  implicit val rrOperationReuse: Reusability[ResourceRunOperation] =
    Reusability.byRef
  implicit val availableTabsReuse: Reusability[AvailableTab] = Reusability.byEq
  implicit val userDetailsReuse: Reusability[UserDetails]    = Reusability.byEq
  implicit val usrNotReuse: Reusability[UserNotificationState] =
    Reusability.byEq
  implicit val dcAddReuse: Reusability[AddDayCalOperation] = Reusability.byRef
  implicit val qoReuse: Reusability[QueueOperations]       = Reusability.byEq
  implicit val qfReuse: Reusability[CalQueueControlFocus]  = Reusability.byEq
  implicit val cqfReuse: Reusability[CalQueueFocus]        = Reusability.byEq
  implicit val qidReuse: Reusability[QueueId]              = Reusability.byEq
  implicit val bexReuse: Reusability[BatchExecState]       = Reusability.byRef
  implicit val soundReuse: Reusability[SoundSelection]     = Reusability.byRef

  implicit val resMap: Reusability[Map[Resource, ResourceRunOperation]] =
    Reusability.map
  implicit val resSMap: Reusability[SortedMap[Resource, ResourceRunOperation]] =
    Reusability.by(_.toMap)
}
