// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.{ Eq, Show }
import cats.implicits._

sealed abstract class FPUMode(val label: String)
  extends Product with Serializable

object FPUMode {

  case object BuiltIn extends FPUMode("BUILTIN")
  case object Custom  extends FPUMode("CUSTOM_MASK")

  val all: List[FPUMode] =
    List(BuiltIn, Custom)

  implicit val equal: Eq[FPUMode] =
    Eq.fromUniversalEquals

  implicit val show: Show[FPUMode] =
    Show.fromToString

  def fromString(s: String): Option[FPUMode] =
    all.find(_.label === s)

}