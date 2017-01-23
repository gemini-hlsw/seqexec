package gem

import doobie.contrib.postgresql.syntax._
import doobie.imports._

import scalaz._
import Scalaz._

package object seqimporter {
  type ConfigMap = String ==>> String

  val EmptyConfigMap = ==>>.empty[String, String]

  def ignoreUniqueViolation(fa: ConnectionIO[Int]): ConnectionIO[Int] =
    for {
      s <- HC.setSavepoint
      n <- fa.onUniqueViolation(HC.rollback(s).as(0))
      _ <- HC.releaseSavepoint(s)
    } yield n
}
