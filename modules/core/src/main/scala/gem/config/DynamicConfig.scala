package gem
package config

import gem.enum._

import java.time.Duration

sealed trait SmartGcalKey

sealed abstract class DynamicConfig extends Product with Serializable {

  type I <: Instrument with Singleton
  def instrument: I

  def smartGcalKey: Option[SmartGcalKey] =
    this match {
      case f2: F2Config     => Some(f2.key)
      case _ => None
    }

}
object DynamicConfig {
  type Aux[I0] = DynamicConfig { type I = I0 }
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
  readMode:      F2ReadMode,
  windowCover:   F2WindowCover
) extends DynamicConfig {

  type I = Instrument.Flamingos2.type
  def instrument = valueOf[I]

  def key: F2SmartGcalKey =
    F2SmartGcalKey(disperser, filter, fpu)
}
