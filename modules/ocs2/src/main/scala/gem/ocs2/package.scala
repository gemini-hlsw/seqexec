package gem

import scalaz._

package object ocs2 {
  type ConfigMap = String ==>> String

  val EmptyConfigMap = ==>>.empty[String, String]
}
