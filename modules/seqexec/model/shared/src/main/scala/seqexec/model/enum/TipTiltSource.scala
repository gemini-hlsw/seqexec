// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import lucuma.core.util.Enumerated

/** Enumerated type for Tip/Tilt Source. */
sealed trait TipTiltSource extends Product with Serializable

object TipTiltSource {
  case object PWFS1 extends TipTiltSource
  case object PWFS2 extends TipTiltSource
  case object OIWFS extends TipTiltSource
  case object GAOS  extends TipTiltSource

  /** @group Typeclass Instances */
  implicit val TipTiltSourceEnumerated: Enumerated[TipTiltSource] =
    Enumerated.of(PWFS1, PWFS2, OIWFS, GAOS)
}
