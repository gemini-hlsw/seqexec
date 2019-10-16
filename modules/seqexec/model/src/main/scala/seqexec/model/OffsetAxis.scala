// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats._

sealed trait OffsetAxis {
  val configItem: String
}

object OffsetAxis {
  case object AxisP extends OffsetAxis {
    val configItem = "p"
  }
  case object AxisQ extends OffsetAxis {
    val configItem = "q"
  }
  implicit val show: Show[OffsetAxis] = Show.show {
    case AxisP => "p"
    case AxisQ => "q"
  }
}
