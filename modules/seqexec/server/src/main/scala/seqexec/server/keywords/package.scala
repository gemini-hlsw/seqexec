// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.{Eq, Monoid}
import cats.implicits._
import cats.effect.IO
import seqexec.model.dhs.ImageFileId

package keywords {

  /**
    * Clients that can send keywords to a server that could e.g. write them to a file
    */
  trait KeywordsClient[F[_]] {

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): SeqActionF[F, Unit]
  }

  trait DhsInstrument extends KeywordsClient[IO] {
    val dhsClient: DhsClient

    val dhsInstrumentName: String

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): SeqAction[Unit] =
      dhsClient.setKeywords(id, keywords, finalFlag)
  }

  trait GDSInstrument extends KeywordsClient[IO] {
    val gdsClient: GDSClient

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): SeqAction[Unit] =
      gdsClient.setKeywords(id, keywords, finalFlag)
  }

  final case class StandaloneDhsClient(dhsClient: DhsClient)
      extends KeywordsClient[IO] {
    override def setKeywords(id: ImageFileId,
                             keywords: KeywordBag,
                             finalFlag: Boolean): SeqAction[Unit] =
      dhsClient.setKeywords(id, keywords, finalFlag)
  }

  // TODO: Implement the unsigned types, if needed.
  sealed trait KeywordType

  object KeywordType {
    implicit val eq: Eq[KeywordType] = Eq.fromUniversalEquals
  }

  case object TypeInt8    extends KeywordType
  case object TypeInt16   extends KeywordType
  case object TypeInt32   extends KeywordType
  case object TypeFloat   extends KeywordType
  case object TypeDouble  extends KeywordType
  case object TypeBoolean extends KeywordType
  case object TypeString  extends KeywordType

  // The developer uses these classes to define all the typed keywords
  sealed class Keyword[T] protected (val n: String,
                                     val t: KeywordType,
                                     val v: T)
  final case class Int8Keyword(name: String, value: Byte)
      extends Keyword[Byte](name, TypeInt8, value)
  final case class Int16Keyword(name: String, value: Short)
      extends Keyword[Short](name, TypeInt16, value)
  final case class Int32Keyword(name: String, value: Int)
      extends Keyword[Int](name, TypeInt32, value)
  final case class FloatKeyword(name: String, value: Float)
      extends Keyword[Float](name, TypeFloat, value)
  final case class DoubleKeyword(name: String, value: Double)
      extends Keyword[Double](name, TypeDouble, value)
  final case class BooleanKeyword(name: String, value: Boolean)
      extends Keyword[Boolean](name, TypeBoolean, value)
  final case class StringKeyword(name: String, value: String)
      extends Keyword[String](name, TypeString, value)

  // At the end, I want to just pass a list of keywords to be sent to the DHS. I cannot do this with Keyword[T],
  // because I cannot mix different types in a List. But at the end I only care about the value as a String, so I
  // use an internal representation, and offer a class to the developer (KeywordBag) to create the list from typed
  // keywords.

  private[server] final case class InternalKeyword(name: String,
                                                     keywordType: KeywordType,
                                                     value: String)

  object InternalKeyword {
    implicit val eq: Eq[InternalKeyword] = Eq.by(x => (x.name, x.keywordType, x.value))
  }

  final case class KeywordBag(keywords: List[InternalKeyword]) {

    def add[T](k: Keyword[T]): KeywordBag =
      KeywordBag(keywords :+ internalKeywordConvert(k))

    def append(other: KeywordBag): KeywordBag =
      KeywordBag(keywords ::: other.keywords)
  }

  //TODO: Add more apply methods if necessary
  @SuppressWarnings(Array("org.wartremover.warts.Overloading"))
  object KeywordBag {
    def empty: KeywordBag = KeywordBag(List())

    implicit val eq: Eq[KeywordBag] = Eq.by(_.keywords)
    implicit val monoid: Monoid[KeywordBag] = new Monoid[KeywordBag] {
      override def empty: KeywordBag = KeywordBag.empty
      override def combine(a: KeywordBag, b: KeywordBag) = KeywordBag(a.keywords |+| b.keywords)
    }

    def apply(ks: Keyword[_]*): KeywordBag =
      KeywordBag(ks.toList.map(internalKeywordConvert))

  }
}

package object keywords {

  def internalKeywordConvert[_](k: Keyword[_]): InternalKeyword =
    InternalKeyword(k.n, k.t, s"${k.v}")
}
