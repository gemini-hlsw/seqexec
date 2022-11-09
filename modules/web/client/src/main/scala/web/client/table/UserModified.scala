// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import japgolly.scalajs.react.Reusability

sealed trait UserModified extends Product with Serializable
case object IsModified    extends UserModified
case object NotModified   extends UserModified

object UserModified {
  implicit val eq: Eq[UserModified] = Eq.fromUniversalEquals

  implicit val reuse: Reusability[UserModified] = Reusability.byRef

  def fromBool(b: Boolean): UserModified = if (b) IsModified else NotModified
}
