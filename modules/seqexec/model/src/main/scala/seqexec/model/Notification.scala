// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import gem.Observation
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource

sealed trait Notification

object Notification {
  implicit lazy val eq: Eq[Notification] =
    Eq.instance {
      case (a: ResourceConflict, b: ResourceConflict) => a === b
      case (a: InstrumentInUse, b: InstrumentInUse)   => a === b
      case (a: RequestFailed, b: RequestFailed)       => a === b
      case (a: SubsystemBusy, b: SubsystemBusy)       => a === b
      case _                                          => false
    }

  def header(n: Notification): String = n match {
    case ResourceConflict(_)    => "Resource conflict"
    case InstrumentInUse(_, _)  => "Instrument busy"
    case RequestFailed(_)       => "Request failed"
    case SubsystemBusy(_, _, _) => "Resource busy"
  }

  def body(n: Notification): List[String] = n match {
    case ResourceConflict(sid) =>
      List(
        s"There is a conflict trying to run the sequence '${sid.format}'",
        "Possibly another sequence is being executed on the same instrument"
      )
    case InstrumentInUse(sid, ins) =>
      List(
        s"Cannot select sequence '${sid.format}' for instrument '${ins.label}'",
        "Possibly another sequence is being executed on the same instrument"
      )
    case RequestFailed(msgs) =>
      s"Request to the seqexec server failed:" :: msgs

    case SubsystemBusy(_, _, resource) =>
      List(s"Cannot configure ${resource.show}, subsystem busy")
  }
}

// Notification that user tried to run a sequence that used resource already in use
final case class ResourceConflict(sid: Observation.Id) extends Notification
object ResourceConflict {
  implicit lazy val eq: Eq[ResourceConflict] =
    Eq.by(_.sid)
}

// Notification that user tried to select a sequence for an instrument for which a sequence was already running
final case class InstrumentInUse(sid: Observation.Id, ins: Instrument)
    extends Notification

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
final case class SubsystemBusy(oid:      Observation.Id,
                               stepId:   StepId,
                               resource: Resource)
    extends Notification

object SubsystemBusy {
  implicit lazy val eq: Eq[SubsystemBusy] =
    Eq.by(x => (x.oid, x.stepId, x.resource))
}
