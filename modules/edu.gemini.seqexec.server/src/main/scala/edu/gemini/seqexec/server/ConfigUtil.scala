package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{ItemKey, Config}

import scala.reflect.ClassTag
import scalaz._
import Scalaz._

/**
 * Created by jluhrs on 8/17/15.
 */
object ConfigUtil {

  type ExtractFailure = String // for now

  // key syntax: parent / child
  implicit class ItemKeyOps(k: ItemKey) {
    def /(s: String) = new ItemKey(k, s)
  }

  // config syntax: cfg.extract(key).as[Type]
  implicit class ConfigOps(c: Config) {
    final class Extracted(key: ItemKey) {
      def as[A](implicit clazz: ClassTag[A]): ExtractFailure \/ A =
        for {
          v <- Option(c.getItemValue(key)) \/> s"Missing config value for key ${key.getPath}"
          b <- \/.fromTryCatch(clazz.runtimeClass.cast(v).asInstanceOf[A]).leftMap(_.getMessage)
        } yield b
    }
    def extract(key: ItemKey) = new Extracted(key)  
  }

}
