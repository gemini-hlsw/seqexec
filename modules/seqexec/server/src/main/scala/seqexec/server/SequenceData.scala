// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import monocle.macros.Lenses
import seqexec.engine.Sequence
import seqexec.model.Observer

@Lenses
final case class SequenceData(observer: Option[Observer],
                              seqGen: SequenceGen,
                              seq: Sequence.State[IO])

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequenceData
