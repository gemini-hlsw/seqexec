// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui

import cats.Eq

sealed trait Aligned

object Aligned {
  case object None extends Aligned
  case object Left extends Aligned
  case object Center extends Aligned
  case object Right extends Aligned

  implicit val equal: Eq[Aligned] = Eq.fromUniversalEquals
}
