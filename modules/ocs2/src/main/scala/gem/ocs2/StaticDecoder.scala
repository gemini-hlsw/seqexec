// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
      case Instrument.AcqCam     => StaticConfig.AcqCam().right
      case Instrument.Bhros      => StaticConfig.Bhros().right

      case Instrument.Flamingos2 =>
        Legacy.Instrument.MosPreImaging.parse(cm).map(StaticConfig.F2(_))

      case Instrument.GmosN      =>
        parseGmosNorthStaticConfig(cm)

      case Instrument.GmosS      =>
        parseGmosSouthStaticConfig(cm)

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

  private def parseGmosCommonStatic(cm: ConfigMap): PioError \/ Gmos.GmosCommonStaticConfig =
    for {
      d <- Legacy.Instrument.Gmos.Detector.parse(cm)
      m <- Legacy.Instrument.MosPreImaging.parse(cm)
    } yield Gmos.GmosCommonStaticConfig(d, m, None)

  private def parseGmosNorthStaticConfig(cm: ConfigMap): PioError \/ StaticConfig =
    for {
      c <- parseGmosCommonStatic(cm)
      s <- Legacy.Instrument.GmosNorth.StageMode.parse(cm)
    } yield StaticConfig.GmosNorth(c, s)

  private def parseGmosSouthStaticConfig(cm: ConfigMap): PioError \/ StaticConfig =
    for {
      c <- parseGmosCommonStatic(cm)
      s <- Legacy.Instrument.GmosSouth.StageMode.parse(cm)
    } yield StaticConfig.GmosSouth(c, s)

  def decode(n: Node): PioError \/ StaticConfig =
    for {
      cm <- ((n \ "step").headOption \/> missingKey("step")).map(_.toStepConfig)
      i  <- Legacy.Instrument.Instrument.parse(cm)
      sc <- parseStaticConfig(i, cm)
    } yield sc
}
