// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.{ Eq, Show }

sealed abstract class ServerLogLevel(val label: String)
  extends Product with Serializable

object ServerLogLevel {

  case object INFO  extends ServerLogLevel("INFO")
  case object WARN  extends ServerLogLevel("WARNING")
  case object ERROR extends ServerLogLevel("ERROR")

  val all: List[ServerLogLevel] =
    List(INFO, WARN, ERROR)

  implicit val eq: Eq[ServerLogLevel] =
    Eq.fromUniversalEquals

  implicit val show: Show[ServerLogLevel] =
    Show.show(_.label)

}
