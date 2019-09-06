// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats._
import cats.implicits._
import gem.Observation
import gem.enum.KeywordName
import seqexec.model.dhs.ImageFileId

package keywords {
  sealed trait KeywordsBundler[F[_]] {
    def bundleKeywords(ks: List[KeywordBag => F[KeywordBag]]): F[KeywordBag]
  }

  /**
    * Clients that can send keywords to a server that could e.g. write them to a file
    */
  trait KeywordsClient[F[_]] {

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): F[Unit]

    def closeImage(id: ImageFileId): F[Unit]

    def keywordsBundler: KeywordsBundler[F]
  }

  abstract class DhsInstrument[F[_]: Monad] extends KeywordsClient[F] {
    val dhsClient: DhsClient[F]

    val dhsInstrumentName: String

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): F[Unit] =
      dhsClient.setKeywords(id, keywords, finalFlag)

    def closeImage(id: ImageFileId): F[Unit] =
      dhsClient.setKeywords(
        id,
        KeywordBag(StringKeyword(KeywordName.INSTRUMENT, dhsInstrumentName)),
        finalFlag = true)

    def keywordsBundler: KeywordsBundler[F] = DhsInstrument.kb[F](dhsInstrumentName)

  }

  object DhsInstrument {
    def kb[F[_]: Monad](dhsInstrumentName: String): KeywordsBundler[F] = new KeywordsBundler[F] {
      def bundleKeywords(
        ks: List[KeywordBag => F[KeywordBag]]
      ): F[KeywordBag] = {
        val z = Applicative[F].pure(
          KeywordBag(StringKeyword(KeywordName.INSTRUMENT, dhsInstrumentName)))
        ks.foldLeft(z) { case (a, b) => a.flatMap(b) }
      }
    }
  }

  object GdsInstrument {
    def bundleKeywords[F[_]: Monad](
      ks: List[KeywordBag => F[KeywordBag]]
    ): F[KeywordBag] =
      ks.foldLeft(Applicative[F].pure(KeywordBag.empty)) { case (a, b) => a.flatMap(b) }

    def kb[F[_]: Monad]: KeywordsBundler[F] = new KeywordsBundler[F] {
      def bundleKeywords(
        ks: List[KeywordBag => F[KeywordBag]]
      ): F[KeywordBag] =
        GdsInstrument.bundleKeywords(ks)
    }
  }

  abstract class GdsInstrument[F[_]: Monad] extends KeywordsClient[F] {
    val gdsClient: GdsClient[F]

    def setKeywords(id: ImageFileId,
                    keywords: KeywordBag,
                    finalFlag: Boolean): F[Unit] =
      gdsClient.setKeywords(id, keywords)

    def closeImage(id: ImageFileId): F[Unit] =
      gdsClient.closeObservation(id)

    def keywordsBundler: KeywordsBundler[F] = GdsInstrument.kb[F]
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
  trait DefaultHeaderValue[A] {
    def default: A
  }

  object DefaultHeaderValue {
    @inline
    def apply[A](implicit instance: DefaultHeaderValue[A]): DefaultHeaderValue[A] = instance

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

    val TrueDefaultValue: DefaultHeaderValue[Boolean] =
      new DefaultHeaderValue[Boolean] {
        val default: Boolean = true
      }

    val FalseDefaultValue: DefaultHeaderValue[Boolean] =
      new DefaultHeaderValue[Boolean] {
        val default: Boolean = false
      }

    /**
     * @typeclass Functor
     */
    implicit val dhvFunctor: Functor[DefaultHeaderValue] =
      new Functor[DefaultHeaderValue] {
        def map[A, B](fa: DefaultHeaderValue[A])(f: A => B): DefaultHeaderValue[B] =
          new DefaultHeaderValue[B] {
            val default: B = f(fa.default)
          }
      }

  }

}

package object keywords {

  // Default values for FITS headers
  val IntDefault: Int         = -9999
  val DoubleDefault: Double   = -9999.0
  val StrDefault: String      = "No Value"
  val Indef: String           = "INDEF"
  val BooleanDefault: Boolean = false

  def intDefault[F[_]: Applicative]: F[Int]         = IntDefault.pure[F]
  def doubleDefault[F[_]: Applicative]: F[Double]   = DoubleDefault.pure[F]
  def strDefault[F[_]: Applicative]: F[String]      = StrDefault.pure[F]
  def boolDefault[F[_]: Applicative]: F[Boolean]    = BooleanDefault.pure[F]
  def listDefault[F[_]: Applicative, A]: F[List[A]] = List.empty[A].pure[F]

  def internalKeywordConvert[_](k: Keyword[_]): InternalKeyword =
    InternalKeyword(k.n, k.t, s"${k.v}")

  implicit class DefaultValueOps[A](a: Option[A])(
      implicit d: DefaultHeaderValue[A]) {
    def orDefault: A = a.getOrElse(d.default)
  }

  implicit class DefaultValueFOps[F[_]: Functor, A: DefaultHeaderValue](
      val v: F[Option[A]]) {
    private val D: DefaultHeaderValue[A] = DefaultHeaderValue[A]

    def orDefault: F[A] = v.map(_.getOrElse(D.default))
  }

  // Keywords are read and they can fail or be missing
  // This Operation will preserve the value if defined or use the default
  // In case it either fails or is empty
  implicit class KeywordValueSafeOps[F[_]: ApplicativeError[?[_], Throwable], A: DefaultHeaderValue](v: F[Option[A]]) {
    private def safeVal: F[Option[A]] = v.attempt.map {
      case Right(a @ Some(_)) => a
      case _                  => None
    }

    def safeValOrDefault: F[A] = safeVal.orDefault
  }

  implicit class SafeDefaultOps[F[_]: ApplicativeError[?[_], Throwable], A: DefaultHeaderValue](v: F[A]) {
    // Check if there is an error reading a value and if there is a failure
    // use the default
    def safeValOrDefault: F[A] =
      v.handleError(_ => DefaultHeaderValue[A].default)
  }

  def buildKeyword[F[_]: MonadError[?[_], Throwable], A: DefaultHeaderValue](get: F[A], name: KeywordName, f: (KeywordName, A) => Keyword[A]): KeywordBag => F[KeywordBag] =
    k => get.safeValOrDefault.map(x => k.add(f(name, x)))
  def buildInt32[F[_]: MonadError[?[_], Throwable]](get: F[Int], name: KeywordName): KeywordBag => F[KeywordBag]       = buildKeyword(get, name, Int32Keyword)
  def buildDouble[F[_]: MonadError[?[_], Throwable]](get: F[Double], name: KeywordName): KeywordBag => F[KeywordBag]   = buildKeyword(get, name, DoubleKeyword)
  def buildBoolean[F[_]: MonadError[?[_], Throwable]](get: F[Boolean], name: KeywordName, ev: DefaultHeaderValue[Boolean]): KeywordBag => F[KeywordBag] = {
    implicit val defaultV = ev
    buildKeyword(get, name, BooleanKeyword)
  }
  def buildString[F[_]: MonadError[?[_], Throwable]](get: F[String], name: KeywordName): KeywordBag => F[KeywordBag]   = buildKeyword(get, name, StringKeyword)

  def sendKeywords[F[_]: MonadError[?[_], Throwable]](
      id: ImageFileId,
      inst: InstrumentSystem[F],
      b: List[KeywordBag => F[KeywordBag]]): F[Unit] =
    (for {
      bag <- inst.keywordsClient.keywordsBundler.bundleKeywords(b)
      _   <- inst.keywordsClient.setKeywords(id, bag, finalFlag = false)
    } yield ())
    .handleError(_ => ()) // In case there is an error ignore them to let other keywords be added

  def dummyHeader[F[_]: Applicative]: Header[F] = new Header[F] {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): F[Unit] =
      Applicative[F].unit
    override def sendAfter(id: ImageFileId): F[Unit] =
      Applicative[F].unit
  }
}
