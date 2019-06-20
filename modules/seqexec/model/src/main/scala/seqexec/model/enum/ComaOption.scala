// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

/** Enumerated type for Coma option. */
sealed trait ComaOption extends Product with Serializable

object ComaOption {
  case object ComaOn  extends ComaOption
  case object ComaOff extends ComaOption

  val all: List[ComaOption] =
    List(ComaOn, ComaOff)

  /** @group Typeclass Instances */
  implicit val CommaOptionEnumerated: Enumerated[ComaOption] =
    new Enumerated[ComaOption] {
      def all = ComaOption.all
      def tag(a: ComaOption): String = a match {
        case ComaOn => "ComaOn"
        case ComaOff => "ComaOff"
      }
    }
}
