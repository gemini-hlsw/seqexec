// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.syntax.all._
import seqexec.model.Observation
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource

sealed trait Notification extends Product with Serializable

object Notification {
  implicit lazy val eq: Eq[Notification] =
    Eq.instance {
      case (a: ResourceConflict, b: ResourceConflict) => a === b
      case (a: InstrumentInUse, b: InstrumentInUse)   => a === b
      case (a: RequestFailed, b: RequestFailed)       => a === b
      case (a: SubsystemBusy, b: SubsystemBusy)       => a === b
      case _                                          => false
    }

  // Notification that user tried to run a sequence that used resource already in use
  final case class ResourceConflict(sid: Observation.Id) extends Notification
  object ResourceConflict {
    implicit lazy val eq: Eq[ResourceConflict] =
      Eq.by(_.sid)
  }

  // Notification that user tried to select a sequence for an instrument for which a sequence was already running
  final case class InstrumentInUse(sid: Observation.Id, ins: Instrument) extends Notification

  object InstrumentInUse {
    implicit lazy val eq: Eq[InstrumentInUse] =
      Eq.by(x => (x.sid, x.ins))
  }

  // Notification that a request to the backend failed
  final case class RequestFailed(msgs: List[String]) extends Notification

  object RequestFailed {
    implicit lazy val eq: Eq[RequestFailed] =
      Eq.by(_.msgs)
  }

  // Notification that a resource configuration failed as the resource was busy
  final case class SubsystemBusy(oid: Observation.Id, stepId: StepId, resource: Resource)
      extends Notification

  object SubsystemBusy {
    implicit lazy val eq: Eq[SubsystemBusy] =
      Eq.by(x => (x.oid, x.stepId, x.resource))
  }
  }

