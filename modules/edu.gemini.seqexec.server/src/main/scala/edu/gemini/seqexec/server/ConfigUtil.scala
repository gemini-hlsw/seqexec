package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{ItemKey, Config}

import java.beans.PropertyDescriptor

import scala.reflect.ClassTag
import scalaz._
import Scalaz._

/**
 * Utility operations to work with Configs from the ODB
 */
object ConfigUtil {

  type ExtractFailure = String // for now

  // key syntax: parent / child
  implicit class ItemKeyOps(val k: ItemKey) extends AnyVal {
    def /(s: String): ItemKey = new ItemKey(k, s)
    def /(p: PropertyDescriptor): ItemKey = /(p.getName)
  }

  final class Extracted private [server] (c: Config, key: ItemKey) {
    def as[A](implicit clazz: ClassTag[A]): ExtractFailure \/ A =
      for {
        v <- Option(c.getItemValue(key)) \/> s"Missing config value for key ${key.getPath}"
        b <- \/.fromTryCatchNonFatal(clazz.runtimeClass.cast(v).asInstanceOf[A]).leftMap(_.getMessage)
      } yield b
  }

  // config syntax: cfg.extract(key).as[Type]
  implicit class ConfigOps(val c: Config) extends AnyVal {
    def extract(key: ItemKey): Extracted = new Extracted(c, key)
  }

}
