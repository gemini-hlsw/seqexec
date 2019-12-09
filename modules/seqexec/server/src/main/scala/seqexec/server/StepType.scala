// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.implicits._
import seqexec.model.enum.Instrument

sealed trait StepType {
  def instrument: Instrument
}

object StepType {
  final case class CelestialObject(override val instrument: Instrument) extends StepType
  final case class Dark(override val instrument: Instrument) extends StepType
  final case class NodAndShuffle(override val instrument: Instrument) extends StepType
  final case class Gems(override val instrument: Instrument) extends StepType
  final case class AltairObs(override val instrument: Instrument) extends StepType
  // Flats or Arcs that can be taken without caring about OI guiding
  final case class FlatOrArc(override val instrument: Instrument) extends StepType
  // Flats or Arcs that must care about OI guiding
  final case class NightFlatOrArc(override val instrument: Instrument) extends StepType
  final case class DarkOrBias(override val instrument: Instrument) extends StepType
  final case class DarkOrBiasNS(override val instrument: Instrument) extends StepType
  final case class ExclusiveDarkOrBias(override val instrument: Instrument) extends StepType
  case object AlignAndCalib extends StepType {
    override val instrument: Instrument = Instrument.Gpi
  }

  implicit val eqStepType: Eq[StepType] = Eq.instance {
    case (CelestialObject(i), CelestialObject(j))         => i === j
    case (Dark(i), Dark(j))                               => i === j
    case (NodAndShuffle(i), NodAndShuffle(j))             => i === j
    case (Gems(i), Gems(j))                               => i === j
    case (AltairObs(i), AltairObs(j))                     => i === j
    case (FlatOrArc(i), FlatOrArc(j))                     => i === j
    case (NightFlatOrArc(i), NightFlatOrArc(j))           => i === j
    case (DarkOrBias(i), DarkOrBias(j))                   => i === j
    case (DarkOrBiasNS(i), DarkOrBiasNS(j))               => i === j
    case (ExclusiveDarkOrBias(i), ExclusiveDarkOrBias(j)) => i === j
    case (AlignAndCalib, AlignAndCalib)                   => true
    case _                                                => false
  }
}
