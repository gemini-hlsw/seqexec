// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.{ Eq, Show }

final case class Operator(value: String)

object Operator {

  val Zero: Operator =
    Operator("")

  implicit val equal: Eq[Operator] =
    Eq.fromUniversalEquals

  implicit val shows: Show[Operator] =
    Show.show(_.value)

}
