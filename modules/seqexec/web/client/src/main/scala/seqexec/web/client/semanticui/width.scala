// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui

import cats.Eq

sealed trait Width

object Width {
  case object None extends Width
  case object One extends Width
  case object Two extends Width
  case object Three extends Width
  case object Four extends Width
  case object Five extends Width
  case object Six extends Width
  case object Seven extends Width
  case object Eight extends Width
  case object Nine extends Width
  case object Ten extends Width
  case object Eleven extends Width
  case object Twelve extends Width
  case object Thirteen extends Width
  case object Fourteen extends Width
  case object Fifteen extends Width
  case object Sixteen extends Width

  implicit val equal: Eq[Width] = Eq.fromUniversalEquals
}
