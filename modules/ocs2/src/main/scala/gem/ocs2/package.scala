package gem

import scala.xml.Node
import scalaz._
import Scalaz._

package object ocs2 {
  type ConfigMap = String ==>> String

  val EmptyConfigMap = ==>>.empty[String, String]

  /** Adds support for parsing steps into ConfigMap.  This is required by the
    * SequenceDecoder and the StaticDecoder.
    */
  implicit class StepNodeOps(step: Node) {

    /** Adds the configuration contained in this step node to the given
      * ConfigMap, updating any values there as necessary.
      */
    def addStepConfig(cm: ConfigMap): ConfigMap = {
      def addSysConfig(cm: ConfigMap, sys: Node): ConfigMap = {
        val sysName = (sys \ "@name").text

        (sys \ "param").foldLeft(cm) { (map, param) =>
          val paramName  = (param \ "@name" ).text
          val paramValue = (param \ "@value").text
          map + (s"$sysName:$paramName" -> paramValue)
        }
      }

      (step \ "system").foldLeft(cm)(addSysConfig)
    }

    /** Creates a ConfigMap containing just the configuration at this step.
      */
    def toStepConfig: ConfigMap =
      addStepConfig(EmptyConfigMap)
  }
}
