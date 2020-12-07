// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed abstract class CloudCover(val toInt: Option[Int], val label: String)
  extends Product with Serializable

object CloudCover {

  case object Unknown   extends CloudCover(none,     "Unknown")
  case object Percent50 extends CloudCover(50.some,  "50%/Clear")
  case object Percent70 extends CloudCover(70.some,  "70%/Cirrus")
  case object Percent80 extends CloudCover(80.some,  "80%/Cloudy")
  case object Any       extends CloudCover(100.some, "Any")

  /** @group Typeclass Instances */
  implicit val CloudCoverEnumerated: Enumerated[CloudCover] =
    Enumerated.of(Unknown, Percent50, Percent70, Percent80, Any)
}
