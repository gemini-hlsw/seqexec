// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import monocle.macros.Lenses
import seqexec.engine.Sequence
import seqexec.model.NodAndShuffleStep.PendingObserveCmd
import seqexec.model.Observer

@Lenses
final case class SequenceData[F[_]](observer: Option[Observer],
                                    seqGen: SequenceGen[F],
                                    seq: Sequence.State[F],
                                    pendingObsCmd: Option[PendingObserveCmd])
