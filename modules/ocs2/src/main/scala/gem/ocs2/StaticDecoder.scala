// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.implicits._

import gem.config._
import gem.enum._
import gem.ocs2.pio._
import gem.ocs2.pio.PioError.missingKey
import gsp.math.Offset

import scala.xml.Node

/** Decoder for the OCS2 static configuration XML.
  */
object StaticDecoder extends PioDecoder[StaticConfig] {

  def decode(n: Node): Either[PioError, StaticConfig] =
    for {
      cm <- ((n \ "step").headOption toRight missingKey("step")).map(_.toStepConfig)
      i  <- Legacy.Instrument.Instrument.parse(cm)
      sc <- parseStaticConfig(i, cm)
    } yield sc


  private def parseStaticConfig(i: Instrument, cm: ConfigMap): Either[PioError, StaticConfig] =
    i match {
      case Instrument.AcqCam     => StaticConfig.AcqCam()  .asRight
      case Instrument.Bhros      => StaticConfig.Bhros()   .asRight

      case Instrument.Flamingos2 => Flamingos2.parse(cm)
      case Instrument.Ghost      => StaticConfig.Ghost()   .asRight
      case Instrument.GmosN      => Gmos.parseNorth(cm)
      case Instrument.GmosS      => Gmos.parseSouth(cm)
      case Instrument.Gnirs      => Gnirs.parse(cm)

      case Instrument.Gpi        => StaticConfig.Gpi()     .asRight
      case Instrument.Gsaoi      => StaticConfig.Gsaoi()   .asRight
      case Instrument.Michelle   => StaticConfig.Michelle().asRight
      case Instrument.Nici       => StaticConfig.Nici()    .asRight
      case Instrument.Nifs       => StaticConfig.Nifs()    .asRight
      case Instrument.Niri       => StaticConfig.Niri()    .asRight
      case Instrument.Phoenix    => StaticConfig.Phoenix() .asRight
      case Instrument.Trecs      => StaticConfig.Trecs()   .asRight
      case Instrument.Visitor    => StaticConfig.Visitor() .asRight
    }

  private object Flamingos2 {
    def parse(cm: ConfigMap): Either[PioError, StaticConfig] =
      Legacy.Instrument.MosPreImaging.parse(cm).map(StaticConfig.Flamingos2(_))
  }

  private object Gmos {
    import gem.config.GmosConfig.{ GmosCommonStaticConfig, GmosCustomRoiEntry, GmosNodAndShuffle }
    import StaticConfig.{ GmosN, GmosS }

    def parseCustomRoiEntry(cm: ConfigMap, index: Int): Either[PioError, Option[GmosCustomRoiEntry]] = {
      import Legacy.Instrument.Gmos._

      (for {
        xMin <- roiXMin(index).oparse(cm)
        yMin <- roiYMin(index).oparse(cm)
        xRng <- roiXRange(index).oparse(cm)
        yRng <- roiYRange(index).oparse(cm)
      } yield GmosCustomRoiEntry.unsafeFromDescription(xMin, yMin, xRng, yRng)).value
    }

    def parseCustomRoiEntries(cm: ConfigMap): Either[PioError, Set[GmosCustomRoiEntry]] =
      (1 to 5).toList.traverse(parseCustomRoiEntry(cm, _)).map(_.flatMap(_.toList).toSet)

    def parseNodAndShuffle(cm: ConfigMap): Either[PioError, Option[GmosNodAndShuffle]] = {
      import Legacy.Instrument.Gmos._

      (for {
        ap <- NsBeamAp.oparse(cm)
        aq <- NsBeamAq.oparse(cm)
        bp <- NsBeamBp.oparse(cm)
        bq <- NsBeamBq.oparse(cm)
        eo <- EOffsetting.oparse(cm)
        sf <- NsShuffle.oparse(cm)
        cy <- NsCycles.oparse(cm)
      } yield GmosNodAndShuffle(Offset(ap, aq), Offset(bp, bq), eo, sf, cy)).value
    }

    def parseCommonStatic(cm: ConfigMap): Either[PioError, GmosCommonStaticConfig] =
      for {
        d <- Legacy.Instrument.Gmos.Detector.parse(cm)
        m <- Legacy.Instrument.MosPreImaging.parse(cm)
        n <- parseNodAndShuffle(cm)
        r <- parseCustomRoiEntries(cm)
      } yield GmosCommonStaticConfig(d, m, n, r)

    def parseNorth(cm: ConfigMap): Either[PioError, StaticConfig] =
      for {
        c <- parseCommonStatic(cm)
        s <- Legacy.Instrument.GmosNorth.StageMode.parse(cm)
      } yield GmosN(c, s)

    def parseSouth(cm: ConfigMap): Either[PioError, StaticConfig] =
      for {
        c <- parseCommonStatic(cm)
        s <- Legacy.Instrument.GmosSouth.StageMode.parse(cm)
      } yield GmosS(c, s)
  }

  private object Gnirs {
    def parse(cm: ConfigMap): Either[PioError, StaticConfig] =
      Legacy.Instrument.Gnirs.WellDepth.parse(cm).map(StaticConfig.Gnirs(_))
  }

}
