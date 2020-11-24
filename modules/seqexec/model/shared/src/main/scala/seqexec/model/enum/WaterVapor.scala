// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed abstract class WaterVapor(val toInt: Option[Int], val label: String)
  extends Product with Serializable

object WaterVapor {

  case object Unknown   extends WaterVapor(none,     "Unknown")
  case object Percent20 extends WaterVapor(20.some,  "20%/Low")
  case object Percent50 extends WaterVapor(50.some,  "50%/Median")
  case object Percent80 extends WaterVapor(80.some,  "85%/High")
  case object Any       extends WaterVapor(100.some, "Any")

  /** @group Typeclass Instances */
  implicit val WaterVaporEnumerated: Enumerated[WaterVapor] =
    Enumerated.of(Unknown, Percent20, Percent50, Percent80, Any)

}
