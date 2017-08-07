// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import gem.config._
import gem.enum._
import gem.math.Offset
import gem.ocs2.pio._
import gem.ocs2.pio.PioError.missingKey

import scala.xml.Node
import scalaz._
import Scalaz._

/** Decoder for the OCS2 static configuration XML.
  */
object StaticDecoder extends PioDecoder[StaticConfig] {

  def decode(n: Node): PioError \/ StaticConfig =
    for {
      cm <- ((n \ "step").headOption \/> missingKey("step")).map(_.toStepConfig)
      i  <- Legacy.Instrument.Instrument.parse(cm)
      sc <- parseStaticConfig(i, cm)
    } yield sc


  private def parseStaticConfig(i: Instrument, cm: ConfigMap): PioError \/ StaticConfig =
    i match {
      case Instrument.AcqCam     => StaticConfig.AcqCam().right
      case Instrument.Bhros      => StaticConfig.Bhros().right

      case Instrument.Flamingos2 => Flamingos2.parse(cm)
      case Instrument.GmosN      => Gmos.parseNorth(cm)
      case Instrument.GmosS      => Gmos.parseSouth(cm)

      case Instrument.Gnirs      => StaticConfig.Gnirs()          .right
      case Instrument.Gpi        => StaticConfig.Gpi()            .right
      case Instrument.Gsaoi      => StaticConfig.Gsaoi()          .right
      case Instrument.Michelle   => StaticConfig.Michelle()       .right
      case Instrument.Nici       => StaticConfig.Nici()           .right
      case Instrument.Nifs       => StaticConfig.Nifs()           .right
      case Instrument.Niri       => StaticConfig.Niri()           .right
      case Instrument.Phoenix    => StaticConfig.Phoenix()        .right
      case Instrument.Trecs      => StaticConfig.Trecs()          .right
      case Instrument.Visitor    => StaticConfig.Visitor()        .right
    }

  private object Flamingos2 {
    def parse(cm: ConfigMap): PioError \/ StaticConfig =
      Legacy.Instrument.MosPreImaging.parse(cm).map(StaticConfig.F2(_))
  }

  private object Gmos {
    import gem.config.Gmos.{ GmosCommonStaticConfig, GmosCustomRoiEntry, GmosNodAndShuffle }
    import StaticConfig.{ GmosNorth, GmosSouth }

    def parseCustomRoiEntry(cm: ConfigMap, index: Int): PioError \/ Option[GmosCustomRoiEntry] = {
      import Legacy.Instrument.Gmos._

      (for {
        xMin <- roiXMin(index).oparse(cm)
        yMin <- roiYMin(index).oparse(cm)
        xRng <- roiXRange(index).oparse(cm)
        yRng <- roiYRange(index).oparse(cm)
      } yield GmosCustomRoiEntry.unsafeFromDescription(xMin, yMin, xRng, yRng)).run
    }

    def parseCustomRoiEntries(cm: ConfigMap): PioError \/ List[GmosCustomRoiEntry] =
      (1 to 5).toList.traverseU(parseCustomRoiEntry(cm, _)).map(_.flatMap(_.toList))

    def parseNodAndShuffle(cm: ConfigMap): PioError \/ Option[GmosNodAndShuffle] = {
      import Legacy.Instrument.Gmos._

      (for {
        ap <- NsBeamAp.oparse(cm)
        aq <- NsBeamAq.oparse(cm)
        bp <- NsBeamBp.oparse(cm)
        bq <- NsBeamBq.oparse(cm)
        eo <- EOffsetting.oparse(cm)
        sf <- NsShuffle.oparse(cm)
        cy <- NsCycles.oparse(cm)
      } yield GmosNodAndShuffle(Offset(ap, aq), Offset(bp, bq), eo, sf, cy)).run
    }

    def parseCommonStatic(cm: ConfigMap): PioError \/ GmosCommonStaticConfig =
      for {
        d <- Legacy.Instrument.Gmos.Detector.parse(cm)
        m <- Legacy.Instrument.MosPreImaging.parse(cm)
        n <- parseNodAndShuffle(cm)
        r <- parseCustomRoiEntries(cm)
      } yield GmosCommonStaticConfig(d, m, n, r)

    def parseNorth(cm: ConfigMap): PioError \/ StaticConfig =
      for {
        c <- parseCommonStatic(cm)
        s <- Legacy.Instrument.GmosNorth.StageMode.parse(cm)
      } yield GmosNorth(c, s)

    def parseSouth(cm: ConfigMap): PioError \/ StaticConfig =
      for {
        c <- parseCommonStatic(cm)
        s <- Legacy.Instrument.GmosSouth.StageMode.parse(cm)
      } yield GmosSouth(c, s)
  }

}
