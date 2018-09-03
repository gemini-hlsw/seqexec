// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.implicits._
import gem.Observation
import seqexec.model.enum.Instrument

sealed trait Notification

object Notification {
  implicit lazy val eq: Eq[Notification] =
    Eq.instance {
      case (a: ResourceConflict, b: ResourceConflict) => a === b
      case (a: InstrumentInUse, b: InstrumentInUse)   => a === b

      case _                                          => false
    }

  def header(n: Notification): String = n match {
    case ResourceConflict(_)           => "Resource conflict"
    case InstrumentInUse(_, _) => "Instrument busy"
  }

  def body(n: Notification): List[String] = n match {
    case ResourceConflict(sid)              => List(
      s"There is a conflict trying to run the sequence '${sid.format}'",
      "Possibly another sequence is being executed on the same instrument"
    )
    case InstrumentInUse(sid, ins) => List(
      s"Cannot select sequence '${sid.format}' for instrument '${ins.label}",
      "Possibly another sequence is being executed on the same instrument"
    )
  }
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
