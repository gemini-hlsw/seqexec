package gem
package config

import gem.enum._

import java.time.Duration

sealed abstract class InstrumentConfig extends Product with Serializable

final case class F2Config(
  fpu:           F2FpUnit,    // instrument:fpu,2-pix pinhole grid
  mosPreimaging: Boolean,     // instrument:mosPreimaging,NO
  exposureTime:  Duration,    // observe:exposureTime,2.0
  filter:        F2Filter,    // instrument:filter,Dark
  lyotWheel:     F2LyotWheel, // instrument:lyotWheel,f/16 (open))
  disperser:     F2Disperser  // instrument:disperser,None
) extends InstrumentConfig

// TODO: temporary, until all instruments are supported
case class GenericConfig(i: Instrument) extends InstrumentConfig
