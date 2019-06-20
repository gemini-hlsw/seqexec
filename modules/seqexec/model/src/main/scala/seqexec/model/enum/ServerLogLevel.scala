// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats._
import gem.util.Enumerated

sealed abstract class ServerLogLevel(val label: String)
  extends Product with Serializable

object ServerLogLevel {

  case object INFO  extends ServerLogLevel("INFO")
  case object WARN  extends ServerLogLevel("WARNING")
  case object ERROR extends ServerLogLevel("ERROR")

  implicit val show: Show[ServerLogLevel] =
    Show.show(_.label)

  val all: List[ServerLogLevel] =
    List(INFO, WARN, ERROR)

  /** @group Typeclass Instances */
  implicit val ServerLogLevelEnumerated: Enumerated[ServerLogLevel] =
    new Enumerated[ServerLogLevel] {
      def all = ServerLogLevel.all
      def tag(a: ServerLogLevel): String = a.label
    }
}
