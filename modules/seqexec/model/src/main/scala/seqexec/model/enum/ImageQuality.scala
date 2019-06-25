// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

sealed abstract class ImageQuality(val toInt: Int, val label: String)
  extends Product with Serializable

object ImageQuality {

  case object Percent20 extends ImageQuality(20,  "20%/Best")
  case object Percent70 extends ImageQuality(70,  "70%/Good")
  case object Percent85 extends ImageQuality(85,  "85%/Poor")
  case object Any       extends ImageQuality(100, "Any")

  /** @group Typeclass Instances */
  implicit val ImageQualityEnumerated: Enumerated[ImageQuality] =
    Enumerated.of(Percent20, Percent70, Percent85, Any)
}
