// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.Show
import monocle.Iso
import monocle.macros.GenIso
import monocle.macros.Lenses

@Lenses
final case class Operator(value: String)

object Operator {

  val Zero: Operator =
    Operator("")

  implicit val equal: Eq[Operator] =
    Eq.fromUniversalEquals

  implicit val shows: Show[Operator] =
    Show.show(_.value)

  val valueI: Iso[Operator, String] = GenIso[Operator, String]

}
