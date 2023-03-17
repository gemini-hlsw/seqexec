// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client.gpi

import cats.Eq
import cats.syntax.all._

sealed trait GpiGuiding extends Product with Serializable

object GpiGuiding {
  case object Guiding    extends GpiGuiding
  case object NotGuiding extends GpiGuiding

  def fromInt(i: Int): GpiGuiding = if (i === 1) Guiding else NotGuiding

  implicit val eqGpiGuiding: Eq[GpiGuiding] = Eq.fromUniversalEquals
}
