package gem
package config

import gem.enum._

import java.time.Duration

sealed trait SmartGcalKey

sealed abstract class InstrumentConfig extends Product with Serializable {
  def smartGcalKey: Option[SmartGcalKey] =
    this match {
      case f2: F2Config     => Some(f2.key)
      case GenericConfig(_) => None
    }
}

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

  def key: F2SmartGcalKey =
    F2SmartGcalKey(disperser, filter, fpu)
}

// TODO: temporary, until all instruments are supported
case class GenericConfig(i: Instrument) extends InstrumentConfig
