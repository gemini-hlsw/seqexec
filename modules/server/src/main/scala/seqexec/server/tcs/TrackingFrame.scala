// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import cats.Eq
import cats.implicits._

sealed trait TrackingFrame

object TrackingFrame {
  case object AzimuthElevation extends TrackingFrame
  case object FK5              extends TrackingFrame
  case object FK4              extends TrackingFrame
  case object Apparent         extends TrackingFrame
  case class Other(name: String) extends TrackingFrame

  implicit var trackingFrameEq: Eq[TrackingFrame] = Eq.instance {
    case (AzimuthElevation, AzimuthElevation) => true
    case (FK5, FK5)                           => true
    case (FK4, FK4)                           => true
    case (Apparent, Apparent)                 => true
    case (Other(a), Other(b))                 => a === b
    case _                                    => false
  }

  def fromString(v: String): TrackingFrame = v match {
    case "AZEL_TOPO" => AzimuthElevation
    case "FK5"       => FK5
    case "FK4"       => FK4
    case "Appt"      => Apparent
    case _           => Other(v)
  }
}
