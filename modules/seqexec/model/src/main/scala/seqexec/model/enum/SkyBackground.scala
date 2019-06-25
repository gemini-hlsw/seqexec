// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

sealed abstract class SkyBackground(val toInt: Int, val label: String)
  extends Product with Serializable

object SkyBackground {

  case object Percent20 extends SkyBackground(20 , "20%/Darkest")
  case object Percent50 extends SkyBackground(50 , "50%/Dark")
  case object Percent80 extends SkyBackground(80 , "80%/Grey")
  case object Any       extends SkyBackground(100, "Any/Bright")

  /** @group Typeclass Instances */
  implicit val SkyBackgroundEnumerated: Enumerated[SkyBackground] =
    Enumerated.of(Percent20, Percent50, Percent80, Any)
}
