// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed trait EpicsHealth extends Product with Serializable

object EpicsHealth {
  case object Good extends EpicsHealth
  case object Bad  extends EpicsHealth
  implicit def fromInt(v: Int): EpicsHealth = if (v === 0) Good else Bad

  implicit val EpicsHealthEnumerated: Enumerated[EpicsHealth] =
    Enumerated.of(Good, Bad)
}
