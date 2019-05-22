// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.enum.Instrument

sealed trait StepType {
  val instrument: Instrument
}

final case class CelestialObject(override val instrument: Instrument) extends StepType
final case class Dark(override val instrument: Instrument) extends StepType
final case class NodAndShuffle(override val instrument: Instrument) extends StepType
final case class Gems(override val instrument: Instrument) extends StepType
final case class AltairObs(override val instrument: Instrument) extends StepType
final case class FlatOrArc(override val instrument: Instrument) extends StepType
final case class DarkOrBias(override val instrument: Instrument) extends StepType
case object AlignAndCalib extends StepType {
  override val instrument: Instrument = Instrument.Gpi
}
