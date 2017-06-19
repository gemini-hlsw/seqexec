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

import gem.enum.Instrument

sealed abstract class StaticConfig {
  type I <: Instrument with Singleton
  def instrument: I
}

object StaticConfig {
  type Aux[I0] = StaticConfig { type I = I0 }
  sealed abstract class Impl[I0 <: Instrument with Singleton](val instrument: I0) extends StaticConfig {
    type I = I0
  }
}

final case class PhoenixStaticConfig()    extends StaticConfig.Impl(Instrument.Phoenix)
final case class MichelleStaticConfig()   extends StaticConfig.Impl(Instrument.Michelle)
final case class GnirsStaticConfig()      extends StaticConfig.Impl(Instrument.Gnirs)
final case class NiriStaticConfig()       extends StaticConfig.Impl(Instrument.Niri)
final case class TrecsStaticConfig()      extends StaticConfig.Impl(Instrument.Trecs)
final case class NiciStaticConfig()       extends StaticConfig.Impl(Instrument.Nici)
final case class NifsStaticConfig()       extends StaticConfig.Impl(Instrument.Nifs)
final case class GpiStaticConfig()        extends StaticConfig.Impl(Instrument.Gpi)
final case class GsaoiStaticConfig()      extends StaticConfig.Impl(Instrument.Gsaoi)
final case class GmosSStaticConfig()      extends StaticConfig.Impl(Instrument.GmosS)
final case class AcqCamStaticConfig()     extends StaticConfig.Impl(Instrument.AcqCam)
final case class GmosNStaticConfig()      extends StaticConfig.Impl(Instrument.GmosN)
final case class BhrosStaticConfig()      extends StaticConfig.Impl(Instrument.Bhros)
final case class VisitorStaticConfig()    extends StaticConfig.Impl(Instrument.Visitor)

final case class F2StaticConfig(mosPreImaging: Boolean) extends StaticConfig {
  type I = Instrument.Flamingos2.type

  def instrument: I = Instrument.Flamingos2

}
