/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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
      case f2: F2DynamicConfig => Some(f2.key)
      case _                   => None
    }

}
object DynamicConfig {
  type Aux[I0] = DynamicConfig { type I = I0 }

  sealed abstract class Impl[I0 <: Instrument with Singleton](val instrument: I0) extends DynamicConfig {
    type I = I0
  }
}

final case class AcqCamDynamicConfig()   extends DynamicConfig.Impl(Instrument.AcqCam)
final case class BhrosDynamicConfig()    extends DynamicConfig.Impl(Instrument.Bhros)
final case class GmosNDynamicConfig()    extends DynamicConfig.Impl(Instrument.GmosN)
final case class GmosSDynamicConfig()    extends DynamicConfig.Impl(Instrument.GmosS)
final case class GnirsDynamicConfig()    extends DynamicConfig.Impl(Instrument.Gnirs)
final case class GpiDynamicConfig()      extends DynamicConfig.Impl(Instrument.Gpi)
final case class GsaoiDynamicConfig()    extends DynamicConfig.Impl(Instrument.Gsaoi)
final case class MichelleDynamicConfig() extends DynamicConfig.Impl(Instrument.Michelle)
final case class NiciDynamicConfig()     extends DynamicConfig.Impl(Instrument.Nici)
final case class NifsDynamicConfig()     extends DynamicConfig.Impl(Instrument.Nifs)
final case class NiriDynamicConfig()     extends DynamicConfig.Impl(Instrument.Niri)
final case class PhoenixDynamicConfig()  extends DynamicConfig.Impl(Instrument.Phoenix)
final case class TrecsDynamicConfig()    extends DynamicConfig.Impl(Instrument.Trecs)
final case class VisitorDynamicConfig()  extends DynamicConfig.Impl(Instrument.Visitor)

final case class F2SmartGcalKey(
  disperser: F2Disperser,
  filter:    F2Filter,
  fpu:       F2FpUnit
) extends SmartGcalKey

final case class F2DynamicConfig(
  disperser:     F2Disperser,
  exposureTime:  Duration,
  filter:        F2Filter,
  fpu:           F2FpUnit,
  lyotWheel:     F2LyotWheel,
  readMode:      F2ReadMode,
  windowCover:   F2WindowCover
) extends DynamicConfig {

  type I = Instrument.Flamingos2.type
  def instrument: I = valueOf[I]

  def key: F2SmartGcalKey =
    F2SmartGcalKey(disperser, filter, fpu)
}
