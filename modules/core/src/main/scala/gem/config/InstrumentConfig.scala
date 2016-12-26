package gem
package config

import gem.enum._

import java.time.Duration

sealed abstract class InstrumentConfig extends Product with Serializable

sealed trait SmartGcalKey

final case class F2SmartGcalKey(
  disperser: F2Disperser,
  filter:    F2Filter,
  fpu:       F2FpUnit
) extends SmartGcalKey

final case class F2Config(
  disperser:     F2Disperser,
  exposureTime:  Duration,
  filter:        F2Filter,
  fpu:           F2FpUnit,
  lyotWheel:     F2LyotWheel,
  mosPreimaging: Boolean,
  readMode:      F2ReadMode,
  windowCover:   F2WindowCover
) extends InstrumentConfig {

  def smartGcalKey: F2SmartGcalKey =
    F2SmartGcalKey(disperser, filter, fpu)
}

// TODO: temporary, until all instruments are supported
case class GenericConfig(i: Instrument) extends InstrumentConfig
