package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{ItemKey, Config}

import scala.reflect.ClassTag
import scalaz._
import Scalaz._

/**
 * Created by jluhrs on 8/17/15.
 * Code graciously lifted from edu.gemini.itc.shared.ConfigExtractor
 */
object ConfigUtil {
  // Extract an optional integer, values in the configuration are Java objects
  def extractOptionalInteger(c: Config, key: ItemKey): Option[Int] =
    extract[java.lang.Integer](c, key).map(_.toInt).toOption

  // Extract a value of the given type from the configuration
  def extract[A](c: Config, key: ItemKey)(implicit clazz: ClassTag[A]): String \/ A =
    extractWithThrowable[A](c, key).leftMap(_.getMessage)

  // Extract a double value from a string in the configuration
  def extractDoubleFromString(c: Config, key: ItemKey): String \/ Double = {
    val v = for {
      s <- extractWithThrowable[String](c, key)
      d <- \/.fromTryCatch(s.toDouble)
    } yield d
    v.leftMap(_.getMessage)
   }

  // Helper method that enforces that whatever we get from the config
  // for the given key is not null and matches the type we expect.
  private def extractWithThrowable[A](c: Config, key: ItemKey)(implicit clazz: ClassTag[A]): Throwable \/ A = {

    def missingKey(key: ItemKey): \/[Throwable, A] =
      new Error(s"Missing config value for key ${key.getPath}").left[A]

    Option(c.getItemValue(key)).fold(missingKey(key)) { v =>
      \/.fromTryCatch(clazz.runtimeClass.cast(v).asInstanceOf[A])
    }

  }

}
