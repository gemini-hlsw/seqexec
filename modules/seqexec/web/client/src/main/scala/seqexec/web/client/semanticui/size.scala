// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui

import cats.Eq
import japgolly.scalajs.react.Reusability

sealed trait Size

object Size {
  case object NotSized extends Size
  case object Tiny extends Size
  case object Mini extends Size
  case object Medium extends Size
  case object Small extends Size
  case object Large extends Size
  case object Big extends Size
  case object Huge extends Size
  case object Massive extends Size

  implicit val equal: Eq[Size] = Eq.fromUniversalEquals
  implicit val reuse: Reusability[Size] = Reusability.byRef[Size]
}
