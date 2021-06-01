// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import lucuma.core.util.Enumerated

sealed trait NodAndShuffleState extends Product with Serializable

// We need to tell Gmos if we are N&S
object NodAndShuffleState {
  // Names taken from the old seqexec
  case object NodShuffle extends NodAndShuffleState
  case object Classic    extends NodAndShuffleState

  /** @group Typeclass Instances */
  implicit val NSStageEnumerated: Enumerated[NodAndShuffleState] =
    Enumerated.of(NodShuffle, Classic)

}
