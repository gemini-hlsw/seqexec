// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import lucuma.core.util.Enumerated

sealed abstract class ServerLogLevel(val label: String)
  extends Product with Serializable

object ServerLogLevel {

  case object INFO  extends ServerLogLevel("INFO")
  case object WARN  extends ServerLogLevel("WARNING")
  case object ERROR extends ServerLogLevel("ERROR")

  /** @group Typeclass Instances */
  implicit val ServerLogLevelEnumerated: Enumerated[ServerLogLevel] =
    Enumerated.of(INFO, WARN, ERROR)
}
