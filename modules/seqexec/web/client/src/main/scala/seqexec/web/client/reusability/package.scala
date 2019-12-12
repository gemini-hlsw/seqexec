// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import diode.data.PotState
import cats.implicits._
import gem.Observation
import gem.util.Enumerated
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Reusability

import scala.collection.immutable.SortedMap
import seqexec.model.enum.Resource
import seqexec.model.enum.ServerLogLevel
import seqexec.model.dhs._
import seqexec.model.{M1GuideConfig, M2GuideConfig, NSRunningState, NSSubexposure, NodAndShuffleStatus, NodAndShuffleStep, Observer, QueueId, SequenceState, StandardStep, Step, StepConfig, StepState, TelescopeGuideConfig, UserDetails}
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.model.UserNotificationState
import seqexec.web.client.model.WebSocketConnection
import seqexec.web.client.model.QueueOperations
import seqexec.web.client.model.TabOperations
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.model.TabSelected
import seqexec.web.client.model.GlobalLog
import seqexec.web.client.circuit._
import seqexec.web.client.model.StepItems.StepStateSummary
import squants.Time
import shapeless.tag.@@

package object reusability {
  implicit def enumeratedReuse[A <: AnyRef: Enumerated]: Reusability[A] =
    Reusability.byRef
  implicit def taggedInt[A]: Reusability[Int @@ A] =
    Reusability.by(x => x: Int)
  implicit val timeReuse: Reusability[Time]                 = Reusability.by(_.toMilliseconds.toLong)
  implicit val imageIdReuse: Reusability[ImageFileId]       = Reusability.byEq
  implicit val stepStateReuse: Reusability[StepState]       = Reusability.byEq
  implicit val obsIdReuse: Reusability[Observation.Id]      = Reusability.byEq
  implicit val observerReuse: Reusability[Observer]         = Reusability.byEq
  implicit val stepConfigReuse: Reusability[StepConfig]     = Reusability.byEq
  val stdStepReuse: Reusability[StandardStep] =
    Reusability.caseClassExcept('config)
  implicit val nsSubexposureReuse: Reusability[NSSubexposure] =
    Reusability.derive[NSSubexposure]
  implicit val nsRunningStateReuse: Reusability[NSRunningState] =
    Reusability.derive[NSRunningState]
  implicit val nsStatus: Reusability[NodAndShuffleStatus] =
    Reusability.derive[NodAndShuffleStatus]
  val nsStepReuse: Reusability[NodAndShuffleStep] =
    Reusability.caseClassExcept('config)
  implicit val stepReuse: Reusability[Step] =
    Reusability {
      case (a: StandardStep, b: StandardStep)           => stdStepReuse.test(a, b)
      case (a: NodAndShuffleStep, b: NodAndShuffleStep) => nsStepReuse.test(a, b)
      case _                                            => false
    }
  implicit val stepStateSnapshotReuse: Reusability[StepStateSummary] =
    Reusability.byEq
  implicit val seqStateReuse: Reusability[SequenceState]    = Reusability.byEq
  implicit val clientStatusReuse: Reusability[ClientStatus] = Reusability.byEq
  implicit val stepTTReuse: Reusability[StepsTableTypeSelection] =
    Reusability.byEq
  implicit val stTbFocusReuse: Reusability[StepsTableFocus] = Reusability.byEq
  implicit val stASFocusReuse: Reusability[StatusAndStepFocus] =
    Reusability.byEq
  implicit val sCFocusReuse: Reusability[SequenceControlFocus] =
    Reusability.byEq
  implicit val tabSelReuse: Reusability[TabSelected] = Reusability.byRef
  implicit val potStateReuse: Reusability[PotState] = Reusability.byRef
  implicit val webSCeuse: Reusability[WebSocketConnection] =
    Reusability.by(_.ws.state)
  implicit val rrOperationReuse: Reusability[ResourceRunOperation] =
    Reusability.byRef
  implicit val availableTabsReuse: Reusability[AvailableTab] = Reusability.byEq
  implicit val userDetailsReuse: Reusability[UserDetails]    = Reusability.byEq
  implicit val usrNotReuse: Reusability[UserNotificationState] =
    Reusability.byEq
  implicit val qoReuse: Reusability[QueueOperations]       = Reusability.byEq
  implicit val qfReuse: Reusability[CalQueueControlFocus]  = Reusability.byEq
  implicit val cqfReuse: Reusability[CalQueueFocus]        = Reusability.byEq
  implicit val qidReuse: Reusability[QueueId]              = Reusability.byEq
  implicit val globalLogReuse: Reusability[GlobalLog]      = Reusability.byEq
  implicit val resMap: Reusability[Map[Resource, ResourceRunOperation]] =
    Reusability.map
  implicit val sllbMap: Reusability[Map[ServerLogLevel, Boolean]] =
    Reusability.map
  implicit val resSMap: Reusability[SortedMap[Resource, ResourceRunOperation]] =
    Reusability.by(_.toMap)
  implicit val tabOpsMap: Reusability[TabOperations] =
    Reusability.byEq
  implicit val m1gReuse: Reusability[M1GuideConfig] =
    Reusability.derive[M1GuideConfig]
  implicit val m2gReuse: Reusability[M2GuideConfig] =
    Reusability.derive[M2GuideConfig]
  implicit val configReuse: Reusability[TelescopeGuideConfig] =
    Reusability.derive[TelescopeGuideConfig]
}
