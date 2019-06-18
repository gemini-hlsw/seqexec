// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import cats.Eq

/** Enumerated type for Tip/Tilt Source. */
sealed trait TipTiltSource extends Product with Serializable

object TipTiltSource {
  case object PWFS1 extends TipTiltSource
  case object PWFS2 extends TipTiltSource
  case object OIWFS extends TipTiltSource
  case object GAOS  extends TipTiltSource

  implicit val eq: Eq[TipTiltSource] = Eq.fromUniversalEquals
}
