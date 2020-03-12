// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.{ Eq, Show }
import cats.implicits._
import monocle.Prism

final case class Observer(value: String)

object Observer {

  val Zero: Observer =
    Observer("")

  implicit val equal: Eq[Observer] =
    Eq.fromUniversalEquals

  implicit val shows: Show[Observer] =
    Show.show(_.value)

  val valueP: Prism[Observer, String] = Prism[Observer, String](s => s.value.some)(Observer.apply)
}

