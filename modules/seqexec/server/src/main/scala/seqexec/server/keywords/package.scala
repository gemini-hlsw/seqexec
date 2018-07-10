// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

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
  sealed case class KeywordType protected (str: String, gdsType: String)
  object TypeInt8    extends KeywordType("INT8", "INT")
  object TypeInt16   extends KeywordType("INT16", "INT")
  object TypeInt32   extends KeywordType("INT32", "INT")
  object TypeFloat   extends KeywordType("FLOAT", "DOUBLE")
  object TypeDouble  extends KeywordType("DOUBLE", "DOUBLE")
  object TypeBoolean extends KeywordType("BOOLEAN", "BOOLEAN")
  object TypeString  extends KeywordType("STRING", "STRING")

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

  private[keywords] final case class InternalKeyword(name: String,
                                                     keywordType: KeywordType,
                                                     value: String)

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

    def apply[A](k1: Keyword[A]): KeywordBag =
      KeywordBag(List(internalKeywordConvert(k1)))

    def apply[A, B](k1: Keyword[A], k2: Keyword[B]): KeywordBag =
      KeywordBag(List(internalKeywordConvert(k1), internalKeywordConvert(k2)))

    def apply[A, B, C](k1: Keyword[A],
                       k2: Keyword[B],
                       k3: Keyword[C]): KeywordBag =
      KeywordBag(
        List(internalKeywordConvert(k1),
             internalKeywordConvert(k2),
             internalKeywordConvert(k3)))

    def apply[A, B, C, D](k1: Keyword[A],
                          k2: Keyword[B],
                          k3: Keyword[C],
                          k4: Keyword[D]): KeywordBag =
      KeywordBag(
        List(internalKeywordConvert(k1),
             internalKeywordConvert(k2),
             internalKeywordConvert(k3),
             internalKeywordConvert(k4)))

    def apply[A, B, C, D, E](k1: Keyword[A],
                             k2: Keyword[B],
                             k3: Keyword[C],
                             k4: Keyword[D],
                             k5: Keyword[E]): KeywordBag =
      KeywordBag(
        List(internalKeywordConvert(k1),
             internalKeywordConvert(k2),
             internalKeywordConvert(k3),
             internalKeywordConvert(k4),
             internalKeywordConvert(k5)))

    def apply[A, B, C, D, E, F](k1: Keyword[A],
                                k2: Keyword[B],
                                k3: Keyword[C],
                                k4: Keyword[D],
                                k5: Keyword[E],
                                k6: Keyword[F]): KeywordBag =
      KeywordBag(
        List(
          internalKeywordConvert(k1),
          internalKeywordConvert(k2),
          internalKeywordConvert(k3),
          internalKeywordConvert(k4),
          internalKeywordConvert(k5),
          internalKeywordConvert(k6)
        ))
  }
}

package object keywords {

  def internalKeywordConvert[T](k: Keyword[T]): InternalKeyword =
    InternalKeyword(k.n, k.t, s"${k.v}")
}
