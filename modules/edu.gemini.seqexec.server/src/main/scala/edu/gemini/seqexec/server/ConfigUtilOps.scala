package edu.gemini.seqexec.server

import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
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
  case class ContentError(msg: String) extends ExtractFailure

  def explain(e: ExtractFailure): String = e match {
    case KeyNotFound(k)          => s"Missing config value for key ${k.getPath}"
    case ConversionError(k, msg) => s"Error reading key ${k.getPath}: $msg"
    case ContentError(msg)       => s"Logical error: $msg"
  }

  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  implicit class TrySeqed[A] private [server] (r: ExtractFailure \/ A) {
    def asTrySeq: TrySeq[A] = r.leftMap(explainExtractError)
  }

  // key syntax: parent / child
  implicit class ItemKeyOps(val k: ItemKey) extends AnyVal {
    def /(s: String): ItemKey = new ItemKey(k, s)
    def /(p: PropertyDescriptor): ItemKey = /(p.getName)
  }

  trait ExtractItem[A] {
    def itemValue(a: A, key: ItemKey): Option[AnyRef]
  }

  implicit val ConfigExtractItem = new ExtractItem[Config] {
    override def itemValue(c: Config, key: ItemKey): Option[AnyRef] = Option(c.getItemValue(key))
  }

  implicit val ConfigSequenceExtractItem = new ExtractItem[ConfigSequence] {
    override def itemValue(c: ConfigSequence, key: ItemKey): Option[AnyRef] = Option(c.getItemValue(0, key))
  }

  final class Extracted[C] private [server] (c: C, key: ItemKey)(implicit ei: ExtractItem[C]) {
    def as[A](implicit clazz: ClassTag[A]): ExtractFailure \/ A =
      for {
        v <- ei.itemValue(c, key) \/> KeyNotFound(key)
        b <- \/.fromTryCatchNonFatal(clazz.runtimeClass.cast(v).asInstanceOf[A]).leftMap(e => ConversionError(key,e.getMessage))
      } yield b
  }

  implicit class ConfigOps(val c: Config) extends AnyVal {
    // config syntax: cfg.extract(key).as[Type]
    def extract(key: ItemKey): Extracted[Config] = new Extracted(c, key)

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

  implicit class ConfigSequenceOps(val c: ConfigSequence) extends AnyVal {
    // config syntax: cfg.extract(key).as[Type]
    def extract(key: ItemKey): Extracted[ConfigSequence] = new Extracted(c, key)
  }
}
