package gem

import scalaz._

package object seqimporter {
  type ConfigMap = String ==>> String

  val EmptyConfigMap = ==>>.empty[String, String]
}
