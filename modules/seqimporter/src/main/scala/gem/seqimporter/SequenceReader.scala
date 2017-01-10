package gem.seqimporter

import gem.Step
import gem.config.InstrumentConfig

import scala.xml.{Elem, Node}

import scalaz._
import Scalaz._

/** Parses an OCS2 XML sequence representation into a
  * `List[Step[(InstrumentConfig, Boolean)]]` where the `Boolean` is `true` iff
  * the step has been executed.
  */
object SequenceReader {

  // Extracts the steps int the XML sequence to a simple list of Maps where
  // each Map is all the keys and values that apply to the step.
  private def configMaps(xml: Elem): List[ConfigMap] = {
    def extractSystem(configMap: ConfigMap, sys: Node): ConfigMap = {
      val sysName = (sys \ "@name").text

      (configMap /: (sys \ "param")) { (map, param) =>
        val paramName  = (param \ "@name").text
        val paramValue = (param \ "@value").text
        map + (s"$sysName:$paramName" -> paramValue)
      }
    }

    def extractStep(configMap: ConfigMap, step: Node): ConfigMap =
      (configMap /: (step \ "system")) { extractSystem }

    (xml \ "step").toList.scanLeft(EmptyConfigMap) { extractStep }.tail
  }

  def unsafeParse(xml: Elem): List[Step[(InstrumentConfig, Boolean)]] =
    configMaps(xml).map(ConfigReader.unsafeStep)
}
