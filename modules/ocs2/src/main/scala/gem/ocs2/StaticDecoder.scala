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

package gem.ocs2

import gem.config._
import gem.enum._
import gem.ocs2.pio.{PioDecoder, PioError}
import gem.ocs2.pio.PioError.missingKey

import scala.xml.Node

import scalaz._
import Scalaz._

/** Decoder for the OCS2 static configuration XML.
  */
object StaticDecoder extends PioDecoder[StaticConfig] {

  private def parseStaticConfig(i: Instrument, cm: ConfigMap): PioError \/ StaticConfig =
    i match {
      case Instrument.AcqCam     => AcqCamStaticConfig().right
      case Instrument.Bhros      => BhrosStaticConfig().right

      case Instrument.Flamingos2 =>
        Legacy.Instrument.MosPreImaging.parse(cm).map(F2StaticConfig(_))

      case Instrument.GmosN      => GmosNStaticConfig().right
      case Instrument.GmosS      => GmosSStaticConfig().right
      case Instrument.Gnirs      => GnirsStaticConfig().right
      case Instrument.Gpi        => GpiStaticConfig().right
      case Instrument.Gsaoi      => GsaoiStaticConfig().right
      case Instrument.Michelle   => MichelleStaticConfig().right
      case Instrument.Nici       => NiciStaticConfig().right
      case Instrument.Nifs       => NifsStaticConfig().right
      case Instrument.Niri       => NiriStaticConfig().right
      case Instrument.Phoenix    => PhoenixStaticConfig().right
      case Instrument.Trecs      => TrecsStaticConfig().right
      case Instrument.Visitor    => VisitorStaticConfig().right
    }

  def decode(n: Node): PioError \/ StaticConfig =
    for {
      cm <- ((n \ "step").headOption \/> missingKey("step")).map(_.toStepConfig)
      i  <- Legacy.Instrument.Instrument.parse(cm)
      sc <- parseStaticConfig(i, cm)
    } yield sc
}
