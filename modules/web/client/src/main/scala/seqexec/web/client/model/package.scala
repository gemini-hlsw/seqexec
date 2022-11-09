// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.Eq
import cats.syntax.all._
import diode.data._
import org.scalajs.dom.WebSocket
import seqexec.web.client.model.SectionVisibilityState._

package object model {
  implicit val eqWebSocket: Eq[WebSocket] =
    Eq.by { x =>
      (x.url, x.protocol, x.readyState)
    }

  implicit def eqPot[A: Eq]: Eq[Pot[A]] = Eq.instance {
    case (Empty, Empty)                           => true
    case (Unavailable, Unavailable)               => true
    case (Pending(a), Pending(b))                 => a === b
    case (Ready(a), Ready(b))                     => a === b
    case (PendingStale(a, b), PendingStale(c, d)) => a === c && b === d
    case (Failed(a), Failed(b))                   => a == b
    case (FailedStale(a, b), FailedStale(c, d))   => a === c && b == d
    case _                                        => false
  }

  implicit class SectionVisibilityStateOps(val s: SectionVisibilityState) extends AnyVal {
    def toggle: SectionVisibilityState = s match {
      case SectionOpen   => SectionClosed
      case SectionClosed => SectionOpen
    }
  }

}
