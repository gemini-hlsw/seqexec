// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import scala.collection.immutable.SortedMap

import diode.data.PotState
import gem.Observation
import lucuma.core.util.Enumerated
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react.Reusability
import react.common._
import react.semanticui.SemanticColor
import react.semanticui.SemanticSize
import seqexec.model._
import seqexec.model.dhs._
import seqexec.model.enum.Resource
import seqexec.model.enum.ServerLogLevel
import seqexec.web.client.circuit._
import seqexec.web.client.model.AvailableTab
import seqexec.web.client.model.ClientStatus
import seqexec.web.client.model.GlobalLog
import seqexec.web.client.model.QueueOperations
import seqexec.web.client.model.ResourceRunOperation
import seqexec.web.client.model.StepItems.StepStateSummary
import seqexec.web.client.model.TabOperations
import seqexec.web.client.model.TabSelected
import seqexec.web.client.model.UserNotificationState
import seqexec.web.client.model.UserPromptState
import seqexec.web.client.model.WebSocketConnection
import shapeless.tag.@@
import squants.Time

package object reusability {
  implicit def oldEnumeratedReuse[A <: AnyRef: gem.util.Enumerated]: Reusability[A] =
    Reusability.byRef
  implicit def enumeratedReuse[A <: AnyRef: Enumerated]: Reusability[A]             =
    Reusability.byRef
  implicit def taggedInt[A]: Reusability[Int @@ A]                                  =
    Reusability.by(x => x: Int)
  implicit val timeReuse: Reusability[Time]                                         = Reusability.by(_.toMilliseconds.toLong)
  implicit val imageIdReuse: Reusability[ImageFileId]                               = Reusability.byEq
  implicit val stepStateReuse: Reusability[StepState]                               = Reusability.byEq
  implicit val obsIdReuse: Reusability[Observation.Id]                              = Reusability.byEq
  implicit val observerReuse: Reusability[Observer]                                 = Reusability.byEq
  implicit val operatorReuse: Reusability[Operator]                                 = Reusability.byEq
  implicit val colorReuse: Reusability[SemanticColor]                               = Reusability.by(_.toJs)
  implicit val cssReuse: Reusability[Css]                                           = Reusability.by(_.htmlClass)
  implicit val stepConfigReuse: Reusability[StepConfig]                             = Reusability.byEq
  val stdStepReuse: Reusability[StandardStep]                                       =
    Reusability.caseClassExcept("config")
  implicit val nsSubexposureReuse: Reusability[NSSubexposure]                       =
    Reusability.derive[NSSubexposure]
  implicit val nsRunningStateReuse: Reusability[NSRunningState]                     =
    Reusability.derive[NSRunningState]
  implicit val nsStatus: Reusability[NodAndShuffleStatus]                           =
    Reusability.derive[NodAndShuffleStatus]
  val nsStepReuse: Reusability[NodAndShuffleStep]                                   =
    Reusability.caseClassExcept("config")
  implicit val stepReuse: Reusability[Step]                                         =
    Reusability {
      case (a: StandardStep, b: StandardStep)           => stdStepReuse.test(a, b)
      case (a: NodAndShuffleStep, b: NodAndShuffleStep) => nsStepReuse.test(a, b)
      case _                                            => false
    }
  implicit val stepStateSnapshotReuse: Reusability[StepStateSummary]                =
    Reusability.byEq
  implicit val seqStateReuse: Reusability[SequenceState]                            = Reusability.byEq
  implicit val clientStatusReuse: Reusability[ClientStatus]                         = Reusability.byEq
  implicit val stepTTReuse: Reusability[StepsTableTypeSelection]                    =
    Reusability.byEq
  implicit val stTbFocusReuse: Reusability[StepsTableFocus]                         = Reusability.byEq
  implicit val stASFocusReuse: Reusability[StatusAndStepFocus]                      =
    Reusability.byEq
  implicit val sCFocusReuse: Reusability[SequenceControlFocus]                      =
    Reusability.byEq
  implicit val tabSelReuse: Reusability[TabSelected]                                = Reusability.byRef
  implicit val potStateReuse: Reusability[PotState]                                 = Reusability.byRef
  implicit val webSCeuse: Reusability[WebSocketConnection]                          =
    Reusability.by(_.ws.state)
  implicit lazy val rrOperationReuse: Reusability[ResourceRunOperation]             =
    Reusability.derive
  implicit val availableTabsReuse: Reusability[AvailableTab]                        = Reusability.byEq
  implicit val userDetailsReuse: Reusability[UserDetails]                           = Reusability.byEq
  implicit val usrPromptReuse: Reusability[UserPromptState]                         = Reusability.byEq
  implicit val usrNotReuse: Reusability[UserNotificationState]                      = Reusability.byEq
  implicit val qoReuse: Reusability[QueueOperations]                                = Reusability.byEq
  implicit val qfReuse: Reusability[CalQueueControlFocus]                           = Reusability.byEq
  implicit val cqfReuse: Reusability[CalQueueFocus]                                 = Reusability.byEq
  implicit val qidReuse: Reusability[QueueId]                                       = Reusability.byEq
  implicit val globalLogReuse: Reusability[GlobalLog]                               = Reusability.byEq
  implicit lazy val resMap: Reusability[Map[Resource, ResourceRunOperation]]        =
    Reusability.map
  implicit val sllbMap: Reusability[Map[ServerLogLevel, Boolean]]                   =
    Reusability.map
  implicit lazy val resSMap: Reusability[SortedMap[Resource, ResourceRunOperation]] =
    Reusability.by(_.toMap)
  implicit val tabOpsMap: Reusability[TabOperations]                                =
    Reusability.byEq
  implicit val m1gReuse: Reusability[M1GuideConfig]                                 =
    Reusability.derive[M1GuideConfig]
  implicit val m2gReuse: Reusability[M2GuideConfig]                                 =
    Reusability.derive[M2GuideConfig]
  implicit val configReuse: Reusability[TelescopeGuideConfig]                       =
    Reusability.derive[TelescopeGuideConfig]
  implicit val reuse: Reusability[SemanticSize]                                     = Reusability.byRef[SemanticSize]
}
