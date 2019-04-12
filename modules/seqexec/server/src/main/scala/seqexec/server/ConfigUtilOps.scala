// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.implicits._
import edu.gemini.spModel.config2.Config
import edu.gemini.spModel.config2.ConfigSequence
import edu.gemini.spModel.config2.ItemKey
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY
import edu.gemini.spModel.seqcomp.SeqConfigNames.OBSERVE_KEY
import edu.gemini.spModel.ao.AOConstants.AO_SYSTEM_KEY
import java.beans.PropertyDescriptor
import java.lang.{Integer => JInt}
import scala.reflect.ClassTag
import scala.collection.breakOut
import seqexec.model.enum.SystemName
import seqexec.model.StepConfig
import shapeless.tag
import shapeless.tag.@@

/**
  * Utility operations to work with Configs from the ODB
  */
object ConfigUtilOps {

  /**
    * ExtractFailures are failures that occur when information cannot be extracted correctly.
    * 1. KeyNotFound should be used when an expected key is absent.
    * 2. ConversionError occurs when the given item was found in the step configuration, but cannot
    *   be read as the requested type.
    * 3. ContentError occurs when there is a logical error in the contents of a step configuration.
    *   A typical example would be when the value of one item implies the presence of another, which is missing.
    */
  sealed trait ExtractFailure
  final case class KeyNotFound(key:     ItemKey) extends ExtractFailure
  final case class ConversionError(key: ItemKey, msg: String)
      extends ExtractFailure
  final case class ContentError(msg: String) extends ExtractFailure

  def explain(e: ExtractFailure): String = e match {
    case KeyNotFound(k)          => s"Missing config value for key ${k.getPath}"
    case ConversionError(k, msg) => s"Error reading key ${k.getPath}: $msg"
    case ContentError(msg)       => s"Logical error: $msg"
  }

  def explainExtractError(e: ExtractFailure): SeqexecFailure =
    SeqexecFailure.Unexpected(ConfigUtilOps.explain(e))

  implicit class TrySeqed[A] private[server] (r: Either[ExtractFailure, A]) {
    def asTrySeq: TrySeq[A] = r.leftMap(explainExtractError)
  }

  implicit class EitherOptionOps[A] private[server] (r: Either[ExtractFailure, Option[A]]) {
    def recoverOption: Either[ExtractFailure, Option[A]] = r.recover {
      case KeyNotFound(_) => none[A]
    }
  }

  // key syntax: parent / child
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  implicit class ItemKeyOps(val k: ItemKey) extends AnyVal {
    def /(s: String): ItemKey             = new ItemKey(k, s)
    def /(p: PropertyDescriptor): ItemKey = /(p.getName)
  }

  trait ExtractItem[A] {
    def itemValue(a: A, key: ItemKey): Option[AnyRef]
  }

  implicit val ConfigExtractItem: ExtractItem[Config] =
    (c: Config, key: ItemKey) => Option(c.getItemValue(key))

  implicit val ConfigSequenceExtractItem: ExtractItem[ConfigSequence] =
    (c: ConfigSequence, key: ItemKey) => Option(c.getItemValue(0, key))

  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  final class Extracted[C] private [server] (c: C, key: ItemKey)(implicit ei: ExtractItem[C]) {
    def as[A](implicit clazz: ClassTag[A]): Either[ExtractFailure, A] =
      for {
        v <- Either.fromOption(ei.itemValue(c, key), KeyNotFound(key))
        b <- Either
          .catchNonFatal(clazz.runtimeClass.cast(v).asInstanceOf[A])
          .leftMap(e => ConversionError(key, e.getMessage))
      } yield b
  }

  implicit class ConfigOps(val c: Config) extends AnyVal {
    // config syntax: cfg.extract(key).as[Type]
    def extract(key: ItemKey): Extracted[Config] = new Extracted(c, key)

    // config syntax: cfg.extractAs[Type](key)
    def extractAs[A](key: ItemKey)(
      implicit clazz:     ClassTag[A]): Either[ExtractFailure, A] =
      new Extracted(c, key).as[A]

    // config syntax: cfg.extractInstAs[Type](key)
    def extractInstAs[A](key: PropertyDescriptor)(
      implicit clazz:         ClassTag[A]): Either[ExtractFailure, A] =
      new Extracted(c, INSTRUMENT_KEY / key).as[A]

    // config syntax: cfg.extractInstAs[Type](key)
    def extractObsAs[A](key: PropertyDescriptor)(
      implicit clazz:        ClassTag[A]): Either[ExtractFailure, A] =
      new Extracted(c, OBSERVE_KEY / key).as[A]

    // config syntax: cfg.extractInstAs[Type](key)
    def extractAOAs[A](key: PropertyDescriptor)(
      implicit clazz:        ClassTag[A]): Either[ExtractFailure, A] =
      new Extracted(c, AO_SYSTEM_KEY / key).as[A]

    // config syntax: cfg.toStepConfig
    def toStepConfig: StepConfig =
      c.itemEntries().groupBy(_.getKey.getRoot).map {
        case (subsystem, entries) =>
          SystemName.unsafeFromString(subsystem.getName) ->
            (entries.toList.map { e =>
              (e.getKey.getPath, s"${e.getItemValue}")
            }(breakOut): Map[String, String])
      }

    def extractInstInt[A](
      property: PropertyDescriptor
    ): Either[ExtractFailure, Option[Int @@ A]] =
      c
        .extractInstAs[JInt](property)
        .map(_.toInt.some)
        .map(_.map(tag[A][Int]))
        .recoverOption

  }

  implicit class ConfigSequenceOps(val c: ConfigSequence) extends AnyVal {
    // config syntax: cfgSequence.extract(key).as[Type]
    def extract(key: ItemKey): Extracted[ConfigSequence] = new Extracted(c, key)

    // config syntax: cfgSequence.extractAs[Type](key)
    def extractAs[A](key: ItemKey)(
      implicit clazz:     ClassTag[A]): Either[ExtractFailure, A] =
      new Extracted(c, key).as[A]
  }
}
