// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.implicits._
import gem.util.Enumerated

sealed abstract class Guiding(val configValue: String)
  extends Product with Serializable

object Guiding {

  case object Guide  extends Guiding("guide")
  case object Park   extends Guiding("park")
  case object Freeze extends Guiding("freeze")

  val all: List[Guiding] =
    List(Guide, Park, Freeze)

  def fromString(s: String): Option[Guiding] =
    all.find(_.configValue === s)

  /** @group Typeclass Instances */
  implicit val GuidingEnumerated: Enumerated[Guiding] =
    new Enumerated[Guiding] {
      def all = Guiding.all
      def tag(a: Guiding): String = a.configValue
    }

}
