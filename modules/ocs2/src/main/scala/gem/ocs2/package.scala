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
