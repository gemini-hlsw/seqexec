// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import lucuma.core.util.Enumerated

/** Enumerated type for M1 Source. */
sealed trait M1Source extends Product with Serializable

object M1Source {
  case object PWFS1 extends M1Source
  case object PWFS2 extends M1Source
  case object OIWFS extends M1Source
  case object GAOS  extends M1Source
  case object HRWFS extends M1Source

  /** @group Typeclass Instances */
  implicit val M1SourceEnumerated: Enumerated[M1Source] =
    Enumerated.of(PWFS1, PWFS2, OIWFS, GAOS, HRWFS)

}
