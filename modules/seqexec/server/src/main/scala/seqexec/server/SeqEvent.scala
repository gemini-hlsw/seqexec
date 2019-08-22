// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import gem.Observation
import seqexec.model.enum._
import seqexec.model.UserDetails
import seqexec.model.Observer
import seqexec.model.Operator
import seqexec.model.Conditions
import seqexec.model.Notification
import seqexec.model.ClientId
import seqexec.model.QueueId
import seqexec.model.StepId

sealed trait SeqEvent extends Product with Serializable

object SeqEvent {
  final case class SetOperator(name: Operator, user: Option[UserDetails]) extends SeqEvent
  final case class SetObserver(id: Observation.Id, user: Option[UserDetails], name: Observer) extends SeqEvent
  final case class SetConditions(conditions: Conditions, user: Option[UserDetails]) extends SeqEvent
  final case class LoadSequence(sid: Observation.Id) extends SeqEvent
  final case class UnloadSequence(id: Observation.Id) extends SeqEvent
  final case class AddLoadedSequence(instrument: Instrument, sid: Observation.Id, user: UserDetails, clientId: ClientId) extends SeqEvent
  final case class ClearLoadedSequences(user: Option[UserDetails]) extends SeqEvent
  final case class SetImageQuality(iq: ImageQuality, user: Option[UserDetails]) extends SeqEvent
  final case class SetWaterVapor(wv: WaterVapor, user: Option[UserDetails]) extends SeqEvent
  final case class SetSkyBackground(wv: SkyBackground, user: Option[UserDetails]) extends SeqEvent
  final case class SetCloudCover(cc: CloudCover, user: Option[UserDetails]) extends SeqEvent
  final case class NotifyUser(memo: Notification, clientID: ClientId) extends SeqEvent
  final case class StartQueue(qid: QueueId, clientID: ClientId) extends SeqEvent
  final case class StopQueue(qid: QueueId, clientID: ClientId) extends SeqEvent
  final case class UpdateQueueAdd(qid: QueueId, seqs: List[Observation.Id]) extends SeqEvent
  final case class UpdateQueueRemove(qid: QueueId, seqs: List[Observation.Id], pos: List[Int]) extends SeqEvent
  final case class UpdateQueueMoved(qid: QueueId, cid: ClientId, oid: Observation.Id, pos: Int) extends SeqEvent
  final case class UpdateQueueClear(qid: QueueId) extends SeqEvent
  final case class StartSysConfig(sid: Observation.Id, stepId: StepId, res: Resource) extends SeqEvent
  final case class Busy(sid: Observation.Id, cid: ClientId) extends SeqEvent
  final case class SequenceStart(sid: Observation.Id, stepId: StepId) extends SeqEvent
  final case class ResourceBusy(sid: Observation.Id, stepId: StepId, res: Resource, clientID: ClientId) extends SeqEvent
  case object NullSeqEvent extends SeqEvent
}
