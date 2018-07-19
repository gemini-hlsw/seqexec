// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.{Eq, Monoid}
import cats.implicits._
import cats.effect.IO
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId

package keywords {

  /**
    * Clients that can send keywords to a server that could e.g. write them to a file
    */
  trait KeywordsClient[F[_]] {

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): SeqActionF[F, Unit]

    def closeImage(id: ImageFileId): SeqActionF[F, Unit]

    def bundleKeywords(
        ks: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[KeywordBag]
  }

  trait DhsInstrument extends KeywordsClient[IO] {
    val dhsClient: DhsClient

    val dhsInstrumentName: String

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): SeqAction[Unit] =
      dhsClient.setKeywords(id, keywords, finalFlag)

    def closeImage(id: ImageFileId): SeqAction[Unit] =
      dhsClient.setKeywords(
        id,
        KeywordBag(StringKeyword(KeywordName.INSTRUMENT, dhsInstrumentName)),
        finalFlag = true)

    def bundleKeywords(ks: List[KeywordBag => SeqAction[KeywordBag]])
      : SeqAction[KeywordBag] = {
      val z = SeqAction(
        KeywordBag(StringKeyword(KeywordName.INSTRUMENT, dhsInstrumentName)))
      ks.foldLeft(z) { case (a, b) => a.flatMap(b) }
    }
  }

  trait GDSInstrument extends KeywordsClient[IO] {
    val gdsClient: GDSClient

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): SeqAction[Unit] =
      gdsClient.setKeywords(id, keywords)

    def closeImage(id: ImageFileId): SeqAction[Unit] =
      gdsClient.closeObservation(id)

    def bundleKeywords(
        ks: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[KeywordBag] =
      ks.foldLeft(SeqAction(KeywordBag.empty)) { case (a, b) => a.flatMap(b) }

  }

  sealed trait KeywordType extends Product with Serializable

  object KeywordType {
    implicit val eq: Eq[KeywordType] = Eq.fromUniversalEquals

    def dhsKeywordType(k: KeywordType): String = k match {
      case TypeInt8    => "INT8"
      case TypeInt16   => "INT16"
      case TypeInt32   => "INT32"
      case TypeFloat   => "FLOAT"
      case TypeDouble  => "DOUBLE"
      case TypeBoolean => "BOOLEAN"
      case TypeString  => "STRING"
    }

    def gdsKeywordType(k: KeywordType): String = k match {
      case TypeInt8    => "INT"
      case TypeInt16   => "INT"
      case TypeInt32   => "INT"
      case TypeFloat   => "DOUBLE"
      case TypeDouble  => "DOUBLE"
      case TypeBoolean => "BOOLEAN"
      case TypeString  => "STRING"
    }
  }

  case object TypeInt8    extends KeywordType
  case object TypeInt16   extends KeywordType
  case object TypeInt32   extends KeywordType
  case object TypeFloat   extends KeywordType
  case object TypeDouble  extends KeywordType
  case object TypeBoolean extends KeywordType
  case object TypeString  extends KeywordType

  // The developer uses these classes to define all the typed keywords
  sealed class Keyword[T] protected (val n: KeywordName,
                                     val t: KeywordType,
                                     val v: T)
  final case class Int8Keyword(name: KeywordName, value: Byte)
      extends Keyword[Byte](name, TypeInt8, value)
  final case class Int16Keyword(name: KeywordName, value: Short)
      extends Keyword[Short](name, TypeInt16, value)
  final case class Int32Keyword(name: KeywordName, value: Int)
      extends Keyword[Int](name, TypeInt32, value)
  final case class FloatKeyword(name: KeywordName, value: Float)
      extends Keyword[Float](name, TypeFloat, value)
  final case class DoubleKeyword(name: KeywordName, value: Double)
      extends Keyword[Double](name, TypeDouble, value)
  final case class BooleanKeyword(name: KeywordName, value: Boolean)
      extends Keyword[Boolean](name, TypeBoolean, value)
  final case class StringKeyword(name: KeywordName, value: String)
      extends Keyword[String](name, TypeString, value)

  // At the end, I want to just pass a list of keywords to be sent to the DHS. I cannot do this with Keyword[T],
  // because I cannot mix different types in a List. But at the end I only care about the value as a String, so I
  // use an internal representation, and offer a class to the developer (KeywordBag) to create the list from typed
  // keywords.

  private[server] final case class InternalKeyword(name: KeywordName,
                                                   keywordType: KeywordType,
                                                   value: String)

  object InternalKeyword {
    implicit val eq: Eq[InternalKeyword] =
      Eq.by(x => (x.name, x.keywordType, x.value))
  }

  final case class KeywordBag(keywords: List[InternalKeyword]) {

    def add[T](k: Keyword[T]): KeywordBag =
      KeywordBag(keywords :+ internalKeywordConvert(k))

    def append(other: KeywordBag): KeywordBag =
      KeywordBag(keywords ::: other.keywords)
  }

  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  object KeywordBag {
    def empty: KeywordBag = KeywordBag(List())

    implicit val eq: Eq[KeywordBag] = Eq.by(_.keywords)
    implicit val monoid: Monoid[KeywordBag] = new Monoid[KeywordBag] {
      override def empty: KeywordBag = KeywordBag.empty
      override def combine(a: KeywordBag, b: KeywordBag) =
        KeywordBag(a.keywords |+| b.keywords)
    }

    def apply(ks: Keyword[_]*): KeywordBag =
      KeywordBag(ks.toList.map(internalKeywordConvert))

  }

  // A simple typeclass to encapsulate default values
  sealed trait DefaultHeaderValue[A] {
    def default: A
  }

  object DefaultHeaderValue {
    implicit val IntDefaultValue: DefaultHeaderValue[Int] =
      new DefaultHeaderValue[Int] {
        val default: Int = IntDefault
      }
    implicit val DoubleDefaultValue: DefaultHeaderValue[Double] =
      new DefaultHeaderValue[Double] {
        val default: Double = DoubleDefault
      }
    implicit val StrDefaultValue: DefaultHeaderValue[String] =
      new DefaultHeaderValue[String] {
        val default: String = StrDefault
      }
  }

}

package object keywords {

  // Default values for FITS headers
  val IntDefault: Int         = -9999
  val DoubleDefault: Double   = -9999.0
  val StrDefault: String      = "No Value"
  val BooleanDefault: Boolean = false

  def internalKeywordConvert[_](k: Keyword[_]): InternalKeyword =
    InternalKeyword(k.n, k.t, s"${k.v}")

  implicit class AnyValueToSeqAction[A](val v: A) extends AnyVal {
    def toSeqAction: SeqAction[Option[A]] = SeqAction(Some(v))
  }

  implicit class OptionToSeqAction[A](val v: Option[A]) extends AnyVal {
    def toSeqActionO: SeqAction[Option[A]] = SeqAction(v)
  }

  implicit class DefaultValueOps[A](a: Option[A])(
      implicit d: DefaultHeaderValue[A]) {
    def orDefault: A = a.getOrElse(d.default)
  }

  implicit class A2SeqAction[A: DefaultHeaderValue](val v: Option[A]) {
    def toSeqAction: SeqAction[A] = SeqAction(v.orDefault)
  }

  implicit class SeqActionOption2SeqAction[A: DefaultHeaderValue](
      val v: SeqAction[Option[A]]) {
    def orDefault: SeqAction[A] = v.map(_.orDefault)
  }

  def buildKeyword[A](get: SeqAction[A], name: KeywordName, f: (KeywordName, A) => Keyword[A]): KeywordBag => SeqAction[KeywordBag] =
    k => get.map(x => k.add(f(name, x)))
  def buildInt8(get: SeqAction[Byte], name: KeywordName): KeywordBag => SeqAction[KeywordBag]       = buildKeyword(get, name, Int8Keyword)
  def buildInt16(get: SeqAction[Short], name: KeywordName): KeywordBag => SeqAction[KeywordBag]     = buildKeyword(get, name, Int16Keyword)
  def buildInt32(get: SeqAction[Int], name: KeywordName): KeywordBag => SeqAction[KeywordBag]       = buildKeyword(get, name, Int32Keyword)
  def buildFloat(get: SeqAction[Float], name: KeywordName): KeywordBag => SeqAction[KeywordBag]     = buildKeyword(get, name, FloatKeyword)
  def buildDouble(get: SeqAction[Double], name: KeywordName): KeywordBag => SeqAction[KeywordBag]   = buildKeyword(get, name, DoubleKeyword)
  def buildBoolean(get: SeqAction[Boolean], name: KeywordName): KeywordBag => SeqAction[KeywordBag] = buildKeyword(get, name, BooleanKeyword)
  def buildString(get: SeqAction[String], name: KeywordName): KeywordBag => SeqAction[KeywordBag]   = buildKeyword(get, name, StringKeyword)

  def sendKeywords[F[_]](
      id: ImageFileId,
      inst: InstrumentSystem[F],
      b: List[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] =
    for {
      bag <- inst.keywordsClient.bundleKeywords(b)
      _   <- inst.keywordsClient.setKeywords(id, bag, finalFlag = false)
    } yield ()
}
