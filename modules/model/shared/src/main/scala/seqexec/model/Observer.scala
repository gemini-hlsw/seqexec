// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq
import cats.Show
import monocle.Iso
import monocle.macros.GenIso

final case class Observer(value: String)

object Observer {

  val Zero: Observer =
    Observer("")

  implicit val equal: Eq[Observer] =
    Eq.fromUniversalEquals

  implicit val shows: Show[Observer] =
    Show.show(_.value)

  val valueI: Iso[Observer, String] = GenIso[Observer, String]
}

