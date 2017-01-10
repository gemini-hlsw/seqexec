package gem

import scalaz.==>>

package object seqimporter {
  type ConfigMap = String ==>> String

  val EmptyConfigMap = ==>>.empty[String, String]
}
