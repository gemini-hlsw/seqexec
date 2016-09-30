package gem
package config

import gem.enum._

import java.time.Duration

sealed abstract class InstrumentConfig extends Product with Serializable

final case class F2Config(
  fpu:           F2FpUnit,
  mosPreimaging: Boolean,
  exposureTime:  Duration,
  filter:        F2Filter,
  lyotWheel:     F2LyotWheel,
  disperser:     F2Disperser,
  windowCover:   F2WindowCover
) extends InstrumentConfig

// TODO: temporary, until all instruments are supported
case class GenericConfig(i: Instrument) extends InstrumentConfig
