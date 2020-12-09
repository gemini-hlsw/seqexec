// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.syntax.all._
import lucuma.core.util.Enumerated

sealed abstract class Guiding(val configValue: String) extends Product with Serializable

object Guiding {

  case object Guide  extends Guiding("guide")
  case object Park   extends Guiding("park")
  case object Freeze extends Guiding("freeze")

  def fromString(s: String): Option[Guiding] =
    GuidingEnumerated.all.find(_.configValue === s)

  /** @group Typeclass Instances */
  implicit val GuidingEnumerated: Enumerated[Guiding] =
    Enumerated.of(Guide, Park, Freeze)

}
