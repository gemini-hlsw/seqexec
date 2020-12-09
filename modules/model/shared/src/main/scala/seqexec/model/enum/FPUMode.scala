// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed abstract class FPUMode(val label: String) extends Product with Serializable

object FPUMode {

  case object BuiltIn extends FPUMode("BUILTIN")
  case object Custom  extends FPUMode("CUSTOM_MASK")

  def fromString(s: String): Option[FPUMode] =
    FPUModeEnumerated.all.find(_.label === s)

  /** @group Typeclass Instances */
  implicit val FPUModeEnumerated: Enumerated[FPUMode] =
    Enumerated.of(BuiltIn, Custom)
}
