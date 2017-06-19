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

import edu.gemini.spModel.core.{OffsetP, OffsetQ}
import gem._
import gem.config._
import gem.enum._

import gem.ocs2.pio.{PioDecoder, PioError}

import java.time.Duration

import scala.xml.Node
import scalaz._
import Scalaz._

/** Decoder for the OCS2 sequence XML.
  */
object SequenceDecoder extends PioDecoder[List[Step[DynamicConfig]]] {

  def parseInstConfig(i: Instrument, cm: ConfigMap): PioError \/ DynamicConfig =
    i match {
      case Instrument.Flamingos2 =>
        import Legacy.Instrument.Flamingos2._
        for {
          d <- Disperser.parse(cm)
          e <- Legacy.Observe.ExposureTime.cparseOrElse(cm, Duration.ofMillis(0))
          f <- Filter.parse(cm)
          u <- Fpu.parse(cm)
          l <- LyotWheel.parse(cm)
          r <- ReadMode.parse(cm)
          w <- WindowCover.cparseOrElse(cm, F2WindowCover.Close)
        } yield F2DynamicConfig(d, e, f, u, l, r, w)

      case Instrument.AcqCam   => AcqCamDynamicConfig()  .right
      case Instrument.Bhros    => BhrosDynamicConfig()   .right
      case Instrument.GmosN    => GmosNDynamicConfig()   .right
      case Instrument.GmosS    => GmosSDynamicConfig()   .right
      case Instrument.Gnirs    => GnirsDynamicConfig()   .right
      case Instrument.Gpi      => GpiDynamicConfig()     .right
      case Instrument.Gsaoi    => GsaoiDynamicConfig()   .right
      case Instrument.Michelle => MichelleDynamicConfig().right
      case Instrument.Nici     => NiciDynamicConfig()    .right
      case Instrument.Nifs     => NifsDynamicConfig()    .right
      case Instrument.Niri     => NiriDynamicConfig()    .right
      case Instrument.Phoenix  => PhoenixDynamicConfig() .right
      case Instrument.Trecs    => TrecsDynamicConfig()   .right
      case Instrument.Visitor  => VisitorDynamicConfig() .right
    }

  def parseStep(cm: ConfigMap): PioError \/ Step[DynamicConfig] = {
    def go(observeType: String, instrument: DynamicConfig): PioError \/ Step[DynamicConfig] =
      observeType match {
        case "BIAS" =>
          BiasStep(instrument).right

        case "DARK" =>
          DarkStep(instrument).right

        case "OBJECT" | "CAL" =>
          for {
            p <- Legacy.Telescope.P.cparseOrElse(cm, OffsetP.Zero)
            q <- Legacy.Telescope.Q.cparseOrElse(cm, OffsetQ.Zero)
          } yield ScienceStep(instrument, TelescopeConfig(p, q))

        case "ARC" | "FLAT" =>
          import Legacy.Calibration._
          for {
            l <- Lamp.parse(cm)
            f <- Filter.parse(cm)
            d <- Diffuser.parse(cm)
            s <- Shutter.parse(cm)
            e <- ExposureTime.parse(cm)
            c <- Coadds.parse(cm)
          } yield GcalStep(instrument, GcalConfig(l, f, d, s, e, c.toShort))

        case x =>
          PioError.parseError(x, "ObserveType").left
      }

    for {
      o <- Legacy.Observe.ObserveType.parse(cm)
      i <- Legacy.Instrument.Instrument.parse(cm)
      c <- parseInstConfig(i, cm)
      s <- go(o, c)
    } yield s
  }

  // Extracts the steps in the XML sequence to a simple list of Maps where
  // each Map is all the keys and values that apply to the step.
  def decode(n: Node): PioError \/ List[Step[DynamicConfig]] =
    (n \ "step").toList.scanLeft(EmptyConfigMap) { (m, stepNode) =>
      stepNode.addStepConfig(m)
    }.tail.traverseU(parseStep)
}
