// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.Model.Instrument
import edu.gemini.seqexec.model.Model.Instrument.{F2, GNIRS, GmosN, GmosS}

object properties {
  sealed trait InstrumentProperties
  case object Disperser extends InstrumentProperties

  val instrumentProperties: Map[Instrument, Set[InstrumentProperties]] = Map(
    F2 -> Set.empty,
    GmosS -> Set(Disperser),
    GmosN -> Set(Disperser),
    GNIRS -> Set(Disperser)
  )
}
