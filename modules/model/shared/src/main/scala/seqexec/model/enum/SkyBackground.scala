// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed abstract class SkyBackground(val toInt: Option[Int], val label: String)
  extends Product with Serializable

object SkyBackground {

  case object Unknown   extends SkyBackground(none,     "Unknown")
  case object Percent20 extends SkyBackground(20.some , "20%/Darkest")
  case object Percent50 extends SkyBackground(50.some , "50%/Dark")
  case object Percent80 extends SkyBackground(80.some , "80%/Grey")
  case object Any       extends SkyBackground(100.some, "Any/Bright")

  /** @group Typeclass Instances */
  implicit val SkyBackgroundEnumerated: Enumerated[SkyBackground] =
    Enumerated.of(Unknown, Percent20, Percent50, Percent80, Any)
}
