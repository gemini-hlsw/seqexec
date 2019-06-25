// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

sealed abstract class CloudCover(val toInt: Int, val label: String)
  extends Product with Serializable

object CloudCover {

  case object Percent50 extends CloudCover(50,  "50%/Clear")
  case object Percent70 extends CloudCover(70,  "70%/Cirrus")
  case object Percent80 extends CloudCover(80,  "80%/Cloudy")
  case object Any       extends CloudCover(100, "Any")

  /** @group Typeclass Instances */
  implicit val CloudCoverEnumerated: Enumerated[CloudCover] =
    Enumerated.of(Percent50, Percent70, Percent80, Any)
}
