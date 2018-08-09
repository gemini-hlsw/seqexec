// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import seqexec.model.enum.SystemName
import seqexec.model.Model.StepConfig

import java.beans.PropertyDescriptor

import scala.reflect.ClassTag
import scala.collection.breakOut

import cats.implicits._

/**
 * Utility operations to work with Configs from the ODB
 */
object ConfigUtilOps {

  sealed trait ExtractFailure
  final case class KeyNotFound(key: ItemKey) extends ExtractFailure
  final case class ConversionError(key: ItemKey, msg: String) extends ExtractFailure
  final case class ContentError(msg: String) extends ExtractFailure

  def explain(e: ExtractFailure): String = e match {
    case KeyNotFound(k)          => s"Missing config value for key ${k.getPath}"
    case ConversionError(k, msg) => s"Error reading key ${k.getPath}: $msg"
    case ContentError(msg)       => s"Logical error: $msg"
  }

  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  implicit class TrySeqed[A] private [server] (r: Either[ExtractFailure, A]) {
    def asTrySeq: TrySeq[A] = r.leftMap(explainExtractError)
  }

  // key syntax: parent / child
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  implicit class ItemKeyOps(val k: ItemKey) extends AnyVal {
    def /(s: String): ItemKey = new ItemKey(k, s)
    def /(p: PropertyDescriptor): ItemKey = /(p.getName)
  }

  trait ExtractItem[A] {
    def itemValue(a: A, key: ItemKey): Option[AnyRef]
  }

  implicit val ConfigExtractItem: ExtractItem[Config] = (c: Config, key: ItemKey) => Option(c.getItemValue(key))

  implicit val ConfigSequenceExtractItem: ExtractItem[ConfigSequence] = (c: ConfigSequence, key: ItemKey) => Option(c.getItemValue(0, key))

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final class Extracted[C] private [server] (c: C, key: ItemKey)(implicit ei: ExtractItem[C]) {
    def as[A](implicit clazz: ClassTag[A]): Either[ExtractFailure, A] =
      for {
        v <- Either.fromOption(ei.itemValue(c, key), KeyNotFound(key))
        b <- Either.catchNonFatal(clazz.runtimeClass.cast(v).asInstanceOf[A]).leftMap(e => ConversionError(key,e.getMessage))
      } yield b
  }

  implicit class ConfigOps(val c: Config) extends AnyVal {
    // config syntax: cfg.extract(key).as[Type]
    def extract(key: ItemKey): Extracted[Config] = new Extracted(c, key)

    // config syntax: cfg.extractAs[Type](key)
    def extractAs[A](key: ItemKey)(implicit clazz: ClassTag[A]): Either[ExtractFailure, A] =
      new Extracted(c, key).as[A]

    // config syntax: cfg.toStepConfig
    def toStepConfig: StepConfig = {
      c.itemEntries().groupBy(_.getKey.getRoot).map {
        case (subsystem, entries) =>
          SystemName.unsafeFromString(subsystem.getName) ->
            (entries.toList.map {
              e => (e.getKey.getPath, s"${e.getItemValue}")
            }(breakOut): Map[String, String])
      }
    }

  }

  implicit class ConfigSequenceOps(val c: ConfigSequence) extends AnyVal {
    // config syntax: cfgSequence.extract(key).as[Type]
    def extract(key: ItemKey): Extracted[ConfigSequence] = new Extracted(c, key)
  }
}
