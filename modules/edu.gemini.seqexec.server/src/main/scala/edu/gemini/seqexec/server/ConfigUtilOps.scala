package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{Config, ItemKey}
import edu.gemini.seqexec.model.Model.StepConfig

import java.beans.PropertyDescriptor

import scala.reflect.ClassTag
import scala.collection.breakOut

import scalaz._
import Scalaz._

/**
 * Utility operations to work with Configs from the ODB
 */
object ConfigUtilOps {

  sealed trait ExtractFailure
  case class KeyNotFound(key: ItemKey) extends ExtractFailure
  case class ConversionError(key: ItemKey, msg: String) extends ExtractFailure

  def explain(e: ExtractFailure): String = e match {
    case KeyNotFound(k) => s"Missing config value for key ${k.getPath}"
    case ConversionError(k, msg) => s"Error reading key ${k.getPath}: $msg"
  }

  // key syntax: parent / child
  implicit class ItemKeyOps(val k: ItemKey) extends AnyVal {
    def /(s: String): ItemKey = new ItemKey(k, s)
    def /(p: PropertyDescriptor): ItemKey = /(p.getName)
  }

  final class Extracted private [server] (c: Config, key: ItemKey) {
    def as[A](implicit clazz: ClassTag[A]): ExtractFailure \/ A =
      for {
        v <- Option(c.getItemValue(key)) \/> KeyNotFound(key)
        b <- \/.fromTryCatchNonFatal(clazz.runtimeClass.cast(v).asInstanceOf[A]).leftMap(e => ConversionError(key,e.getMessage))
      } yield b
  }

  implicit class ConfigOps(val c: Config) extends AnyVal {
    // config syntax: cfg.extract(key).as[Type]
    def extract(key: ItemKey): Extracted = new Extracted(c, key)

    // config syntax: cfg.toStepConfig
    def toStepConfig: StepConfig = {
      c.itemEntries().groupBy(_.getKey.getRoot).map {
        case (subsystem, entries) =>
          subsystem.getName ->
            (entries.toList.map {
              e => (e.getKey.getPath, e.getItemValue.toString)
            }(breakOut): Map[String, String])
      }
    }
  }
}
