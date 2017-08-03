// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import gem.ocs2.pio.PioError._
import gem.ocs2.pio.{PioError, PioParse}

import scala.reflect.runtime.universe.TypeTag

import scalaz._
import Scalaz._


/** Legacy system (telescope, instrument, observe, calibration) key and parser
  * definitions.
  */
@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object Legacy {
  sealed abstract class System(val system: String) {

    final class Key[A](val name: String, val parseString: PioParse[A])(implicit ev: TypeTag[A]) {

      val path: String = s"$system:$name"

      @SuppressWarnings(Array("org.wartremover.warts.ToString"))
      val tpe:  String = Key.clean(ev.tpe.toString)

      def rawValue(cm: ConfigMap): PioError \/ String =
        cm.lookup(path) \/> missingKey(name)

      def parse(cm: ConfigMap): PioError \/ A =
        rawValue(cm).flatMap { s => parseString(s) \/> parseError(s, tpe) }

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
    val Instrument    = Key("instrument"   )(Parsers.instrument   )
    val MosPreImaging = Key("mosPreimaging")(Parsers.mosPreImaging)

    object Flamingos2 {
      import Parsers.Flamingos2._
      val Disperser   = Key("disperser"  )(disperser  )
      val Filter      = Key("filter"     )(filter     )
      val Fpu         = Key("fpu"        )(fpu        )
      val LyotWheel   = Key("lyotWheel"  )(lyotWheel  )
      val ReadMode    = Key("readMode"   )(readMode   )
      val WindowCover = Key("windowCover")(windowCover)
    }

    object Gmos {
      import Parsers.Gmos._
      val Adc             = Key("adc"                    )(adc            )
      val AmpCount        = Key("ampCount"               )(ampCount       )
      val AmpGain         = Key("gainChoice"             )(ampGain        )
      val AmpReadMode     = Key("ampReadMode"            )(ampReadMode    )
      val BuiltinRoi      = Key("builtinROI"             )(builtinRoi     )
      val CustomMaskMdf   = Key("fpuCustomMask"          )(PioParse.string)
      val CustomSlitWidth = Key("customSlitWidth"        )(customSlitWidth)
      val Detector        = Key("detectorManufacturer"   )(detector       )
      val DisperserOrder  = Key("disperserOrder"         )(disperserOrder )
      val DisperserLambda = Key("disperserLambda"        )(disperserLambda)
      val Dtax            = Key("dtaXOffset"             )(dtax           )
      val EOffsetting     = Key("useElectronicOffsetting")(nsEOffsetting  )
      val NsBeamAp        = Key("nsBeamA-p"              )(Parsers.offsetP)
      val NsBeamAq        = Key("nsBeamA-q"              )(Parsers.offsetQ)
      val NsBeamBp        = Key("nsBeamB-p"              )(Parsers.offsetP)
      val NsBeamBq        = Key("nsBeamB-q"              )(Parsers.offsetQ)
      val NsCycles        = Key("nsNumCycles"            )(nsCycles       )
      val NsShuffle       = Key("nsDetectorRows"         )(nsShuffle      )
      val XBinning        = Key("ccdXBinning"            )(xBinning       )
      val YBinning        = Key("ccdYBinning"            )(yBinning       )
    }

    object GmosNorth {
      import Parsers.GmosNorth._
      val Disperser       = Key("disperser")(disperser)
      val Filter          = Key("filter"   )(filter   )
      val Fpu             = Key("fpu"      )(fpu      )
      val StageMode       = Key("stageMode")(stageMode)
    }

    object GmosSouth {
      import Parsers.GmosSouth._
      val Disperser       = Key("disperser")(disperser)
      val Filter          = Key("filter"   )(filter   )
      val Fpu             = Key("fpu"      )(fpu      )
      val StageMode       = Key("stageMode")(stageMode)
    }
  }

  object Calibration extends System("calibration") {
    import Parsers.Calibration._
    val Lamp         = Key("lamp"        )(lamp            )
    val Filter       = Key("filter"      )(filter          )
    val Diffuser     = Key("diffuser"    )(diffuser        )
    val Shutter      = Key("shutter"     )(shutter         )
    val ExposureTime = Key("exposureTime")(PioParse.seconds)
    val Coadds       = Key("coadds"      )(PioParse.int    )
  }
}
