// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed abstract class ImageQuality(val toInt: Option[Int], val label: String)
  extends Product with Serializable

object ImageQuality {

  case object Unknown   extends ImageQuality(none,     "Unknown")
  case object Percent20 extends ImageQuality(20.some,  "20%/Best")
  case object Percent70 extends ImageQuality(70.some,  "70%/Good")
  case object Percent85 extends ImageQuality(85.some,  "85%/Poor")
  case object Any       extends ImageQuality(100.some, "Any")

  /** @group Typeclass Instances */
  implicit val ImageQualityEnumerated: Enumerated[ImageQuality] =
    Enumerated.of(Unknown, Percent20, Percent70, Percent85, Any)
}
