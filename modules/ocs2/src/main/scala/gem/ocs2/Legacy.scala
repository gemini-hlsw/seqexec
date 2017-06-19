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

import gem.ocs2.pio.PioError._
import gem.ocs2.pio.{PioError, PioParse}

import scala.reflect.runtime.universe.TypeTag

import scalaz._
import Scalaz._


/** Legacy system (telescope, instrument, observe, calibration) key and parser
  * definitions.
  */
object Legacy {
  sealed abstract class System(val system: String) {

    final class Key[A](val name: String, val p: PioParse[A])(implicit ev: TypeTag[A]) {
      val path = s"$system:$name"
      val tpe  = Key.clean(ev.tpe.toString)

      def rawValue(cm: ConfigMap): PioError \/ String =
        cm.lookup(path) \/> missingKey(name)

      def parse(cm: ConfigMap): PioError \/ A =
        rawValue(cm).flatMap { s => p(s) \/> parseError(s, tpe) }

      def cparse(cm: ConfigMap): PioError \/ Option[A] =
        parse(cm) match {
          case -\/(MissingKey(_)) => None.right
          case -\/(err)           => err.left
          case \/-(a)             => Some(a).right
        }

      def cparseOrElse(cm: ConfigMap, a: => A): PioError \/ A =
        cparse(cm).map { _.getOrElse(a) }

      override def toString: String =
        s"Key[$tpe]($path)"
    }

    protected object Key {
      // Clean up classnames in toString, which tells you the type of the key, which should be
      // helpful for debugging.
      private def clean(s: String) =
        s.replace("edu.gemini.spModel.core.", "")
         .replace("java.lang.", "")
         .replace("java.time.", "")
         .replace("gem.", "")

      def apply[A: TypeTag](name: String)(parse: PioParse[A]): Key[A] =
        new Key(name, parse)
    }
  }

  case object Telescope extends System("telescope") {
    val P = Key("p")(Parsers.offsetP)
    val Q = Key("q")(Parsers.offsetQ)
  }

  case object Observe extends System("observe") {
    val ObserveType  = Key("observeType" )(PioParse.string )
    val ExposureTime = Key("exposureTime")(PioParse.seconds)
  }

  case object Ocs extends System("ocs") {
    val ObservationId = Key("observationId")(Parsers.obsId)
  }

  case object Instrument extends System("instrument") {
    val Instrument    = Key("instrument"   )(Parsers.instrument)
    val MosPreImaging = Key("mosPreimaging")(Parsers.yesNo     )

    object Flamingos2 {
      val Disperser   = Key("disperser"  )(Parsers.Flamingos2.disperser  )
      val Filter      = Key("filter"     )(Parsers.Flamingos2.filter     )
      val Fpu         = Key("fpu"        )(Parsers.Flamingos2.fpu        )
      val LyotWheel   = Key("lyotWheel"  )(Parsers.Flamingos2.lyotWheel  )
      val ReadMode    = Key("readMode"   )(Parsers.Flamingos2.readMode   )
      val WindowCover = Key("windowCover")(Parsers.Flamingos2.windowCover)
    }
  }

  object Calibration extends System("calibration") {
    val Lamp         = Key("lamp"        )(Parsers.Calibration.lamp    )
    val Filter       = Key("filter"      )(Parsers.Calibration.filter  )
    val Diffuser     = Key("diffuser"    )(Parsers.Calibration.diffuser)
    val Shutter      = Key("shutter"     )(Parsers.Calibration.shutter )
    val ExposureTime = Key("exposureTime")(PioParse.seconds            )
    val Coadds       = Key("coadds"      )(PioParse.int                )
  }
}