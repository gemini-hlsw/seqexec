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
        } yield F2Config(d, e, f, u, l, r, w)

      case _ => sys.error(s"Can't decode config $i, $cm")
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
