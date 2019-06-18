// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2

import cats.implicits._
import gem._
import gem.config._
import gem.enum._
import gem.ocs2.pio._
import gsp.math.Offset

import java.time.Duration

import scala.xml.Node

/** Decoder for the OCS2 sequence XML.
  */
object SequenceDecoder extends PioDecoder[List[Step]] {

  def decode(n: Node): Either[PioError, List[Step]] =
    (n \ "step").toList.scanLeft(EmptyConfigMap) { (m, stepNode) =>
      stepNode.addStepConfig(m)
    }.drop(1).traverse(parseStep)

  private def parseStep(cm: ConfigMap): Either[PioError, Step] = {
    def go(observeType: String, dc: DynamicConfig): Either[PioError, Step] =
      observeType match {
        case "BIAS" =>
          dc.toStep(Step.Base.Bias).asRight

        case "DARK" =>
          dc.toStep(Step.Base.Dark).asRight

        case "OBJECT" | "CAL" =>
          for {
            p <- Legacy.Telescope.P.cparseOrElse(cm, Offset.P.Zero)
            q <- Legacy.Telescope.Q.cparseOrElse(cm, Offset.Q.Zero)
          } yield dc.toStep(Step.Base.Science(TelescopeConfig(p, q)))

        case "ARC" | "FLAT" =>
          import Legacy.Calibration._
          for {
            l <- Lamp.parse(cm)
            f <- Filter.parse(cm)
            d <- Diffuser.parse(cm)
            s <- Shutter.parse(cm)
            e <- ExposureTime.parse(cm)
            c <- CoAdds.parse(cm)
          } yield dc.toStep(Step.Base.Gcal(GcalConfig(l, f, d, s, e, c)))

        case x =>
          PioError.parseError(x, "ObserveType").asLeft
      }

    for {
      o <- Legacy.Observe.ObserveType.cparseOrElse(cm, "OBJECT")
      i <- Legacy.Instrument.Instrument.parse(cm)
      c <- parseInstConfig(i, cm)
      s <- go(o, c)
    } yield s
  }

  private def parseInstConfig(i: Instrument, cm: ConfigMap): Either[PioError, DynamicConfig] =
    i match {
      case Instrument.AcqCam     => DynamicConfig.AcqCam()  .asRight
      case Instrument.Bhros      => DynamicConfig.Bhros()   .asRight

      case Instrument.Flamingos2 => Flamingos2.parse(cm)
      case Instrument.Ghost      => DynamicConfig.Ghost()   .asRight
      case Instrument.GmosN      => Gmos.parseNorth(cm)
      case Instrument.GmosS      => Gmos.parseSouth(cm)
      case Instrument.Gnirs      => Gnirs.parse(cm)

      case Instrument.Gpi        => DynamicConfig.Gpi()     .asRight
      case Instrument.Gsaoi      => DynamicConfig.Gsaoi()   .asRight
      case Instrument.Michelle   => DynamicConfig.Michelle().asRight
      case Instrument.Nici       => DynamicConfig.Nici()    .asRight
      case Instrument.Nifs       => DynamicConfig.Nifs()    .asRight
      case Instrument.Niri       => DynamicConfig.Niri()    .asRight
      case Instrument.Phoenix    => DynamicConfig.Phoenix() .asRight
      case Instrument.Trecs      => DynamicConfig.Trecs()   .asRight
      case Instrument.Visitor    => DynamicConfig.Visitor() .asRight
    }

  private object Flamingos2 {
    def parse(cm: ConfigMap): Either[PioError, DynamicConfig] = {
      import Legacy.Instrument.Flamingos2._
      for {
        d <- Disperser.parse(cm)
        e <- Legacy.Observe.ExposureTime.cparseOrElse(cm, Duration.ofMillis(0))
        f <- Filter.parse(cm)
        u <- Fpu.parse(cm)
        l <- LyotWheel.parse(cm)
        r <- ReadMode.parse(cm)
        w <- WindowCover.cparseOrElse(cm, F2WindowCover.Close)
      } yield DynamicConfig.Flamingos2(d, e, f, u, l, r, w)
    }
  }

  private object Gmos {
    import gem.config.GmosConfig.{ GmosCcdReadout, GmosCommonDynamicConfig, GmosCustomMask, GmosGrating }
    import DynamicConfig.{ GmosN, GmosS }

    def common(cm: ConfigMap): Either[PioError, GmosCommonDynamicConfig] = {
      import Legacy.Instrument.Gmos._

      for {
        x  <- XBinning.parse(cm)
        y  <- YBinning.parse(cm)
        ac <- AmpCount.parse(cm)
        ag <- AmpGain.parse(cm)
        ar <- AmpReadMode.parse(cm)
        dx <- Dtax.parse(cm)
        e  <- Legacy.Observe.ExposureTime.cparseOrElse(cm, Duration.ofMillis(0))
        r  <- Roi.parse(cm)
      } yield GmosCommonDynamicConfig(GmosCcdReadout(x, y, ac, ag, ar), dx, e, r)
    }

    def customMask(cm: ConfigMap): Either[PioError, Option[GmosCustomMask]] = {
      import gem.ocs2.Legacy.Instrument.Gmos.{CustomMaskMdf, CustomSlitWidth}

      (for {
        f <- CustomMaskMdf.oparse(cm)
        s <- PioOptional(CustomSlitWidth.parse(cm))
      } yield GmosCustomMask(f, s)).value
    }

    def parseNorth(cm: ConfigMap): Either[PioError, DynamicConfig] = {
      import Legacy.Instrument.Gmos._
      import Legacy.Instrument.GmosNorth._

      val grating: Either[PioError, Option[GmosGrating[GmosNorthDisperser]]] =
        (for {
          d <- PioOptional(Disperser.parse(cm))
          o <- DisperserOrder.oparse(cm)
          w <- DisperserLambda.oparse(cm)
        } yield GmosGrating(d, o, w)).value

      for {
        c <- common(cm)
        g <- grating
        f <- Filter.parse(cm)
        u <- Fpu.cparse(cm).map(_.flatten)
        m <- customMask(cm)
        fpu = u.map(_.asRight[GmosCustomMask]) orElse m.map(_.asLeft[GmosNorthFpu])
      } yield GmosN(c, g, f, fpu)
    }

    def parseSouth(cm: ConfigMap): Either[PioError, DynamicConfig] = {
      import Legacy.Instrument.Gmos._
      import Legacy.Instrument.GmosSouth._

      val grating: Either[PioError, Option[GmosGrating[GmosSouthDisperser]]] =
        (for {
          d <- PioOptional(Disperser.parse(cm))
          o <- DisperserOrder.oparse(cm)
          w <- DisperserLambda.oparse(cm)
        } yield GmosGrating(d, o, w)).value

      for {
        c <- common(cm)
        g <- grating
        f <- Filter.parse(cm)
        u <- Fpu.cparse(cm).map(_.flatten)
        m <- customMask(cm)
        fpu = u.map(_.asRight[GmosCustomMask]) orElse m.map(_.asLeft[GmosSouthFpu])
      } yield GmosS(c, g, f, fpu)
    }
  }

  private object Gnirs {
    def parse(cm: ConfigMap): Either[PioError, DynamicConfig] = {
      import Legacy.Instrument.Gnirs._
      import gem.config.DynamicConfig.Gnirs.Default

      for {
        a <- AcquisitionMirror.parse(cm)
        b <- Camera.parse(cm)
        c <- CoAdds.parse(cm)
        d <- Decker.cparseOrElse(cm, Default.decker)
        e <- Disperser.parse(cm)
        f <- Legacy.Observe.ExposureTime.cparseOrElse(cm, Default.exposureTime)
        g <- Filter.cparseOrElse(cm, Default.filter)
        h <- Fpu.parse(cm)
        i <- Prism.parse(cm)
        j <- ReadMode.parse(cm)
        k <- Wavelength.parse(cm)
      } yield DynamicConfig.Gnirs(a, b, c, d, e, f, g, h, i, j, k)
    }
  }

}
