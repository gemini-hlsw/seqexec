package edu.gemini.seqexec.server

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.DhsClient._

/**
  * Created by jluhrs on 1/31/17.
  */
trait Header {
  def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit]
  def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit]
}

object Header {

  val IntDefault: Int = -9999
  val DoubleDefault: Double = -9999.0
  val StrDefault = "No Value"
  val BooleanDefault = false

  def buildKeyword[A](get: SeqAction[A], name: String, f: (String, A) => DhsClient.Keyword[A]): KeywordBag => SeqAction[KeywordBag] =
    k => get.map(x => k.add(f(name, x)))
  def buildInt8(get: SeqAction[Byte], name: String ): KeywordBag => SeqAction[KeywordBag]       = buildKeyword(get, name, Int8Keyword)
  def buildInt16(get: SeqAction[Short], name: String ): KeywordBag => SeqAction[KeywordBag]     = buildKeyword(get, name, Int16Keyword)
  def buildInt32(get: SeqAction[Int], name: String ): KeywordBag => SeqAction[KeywordBag]       = buildKeyword(get, name, Int32Keyword)
  def buildFloat(get: SeqAction[Float], name: String ): KeywordBag => SeqAction[KeywordBag]     = buildKeyword(get, name, FloatKeyword)
  def buildDouble(get: SeqAction[Double], name: String ): KeywordBag => SeqAction[KeywordBag]   = buildKeyword(get, name, DoubleKeyword)
  def buildBoolean(get: SeqAction[Boolean], name: String ): KeywordBag => SeqAction[KeywordBag] = buildKeyword(get, name, BooleanKeyword)
  def buildString(get: SeqAction[String], name: String ): KeywordBag => SeqAction[KeywordBag]   = buildKeyword(get, name, StringKeyword)

  private def bundleKeywords(inst: String, ks: Seq[KeywordBag => SeqAction[KeywordBag]]): SeqAction[KeywordBag] = {
    val z = SeqAction(KeywordBag(StringKeyword("instrument", inst)))
    ks.foldRight(z) { case (a,b) => b.flatMap(a) }
  }

  def sendKeywords(id: ImageFileId, inst: String, hs: DhsClient, b: Seq[KeywordBag => SeqAction[KeywordBag]]): SeqAction[Unit] = for {
    bag <- bundleKeywords(inst, b)
    _   <- hs.setKeywords(id, bag, finalFlag = false)
  } yield ()


  object Implicits {
    implicit class String2SeqAction(val v: Option[String]) extends AnyVal {
      def toSeqAction: SeqAction[String] = SeqAction(v.getOrElse(Header.StrDefault))
    }

    implicit class Int2SeqAction(val v: Option[Int]) extends AnyVal {
      def toSeqAction: SeqAction[Int] = SeqAction(v.getOrElse(Header.IntDefault))
    }

    implicit class Double2SeqAction(val v: Option[Double]) extends AnyVal {
      def toSeqAction: SeqAction[Double] = SeqAction(v.getOrElse(Header.DoubleDefault))
    }

    implicit class Boolean2SeqAction(val v: Option[Boolean]) extends AnyVal {
      def toSeqAction: SeqAction[Boolean] = SeqAction(v.getOrElse(Header.BooleanDefault))
    }

    implicit class SeqActionString2SeqAction(val v: SeqAction[Option[String]]) extends AnyVal {
      def orDefault: SeqAction[String] = v.map(_.getOrElse(Header.StrDefault))
    }

    implicit class SeqActionInt2SeqAction(val v: SeqAction[Option[Int]]) extends AnyVal {
      def orDefault: SeqAction[Int] = v.map(_.getOrElse(Header.IntDefault))
    }

    implicit class SeqActionDouble2SeqAction(val v: SeqAction[Option[Double]]) extends AnyVal {
      def orDefault: SeqAction[Double] = v.map(_.getOrElse(Header.DoubleDefault))
    }

    implicit class SeqActionBoolean2SeqAction(val v: SeqAction[Option[Boolean]]) extends AnyVal {
      def orDefault: SeqAction[Boolean] = v.map(_.getOrElse(Header.BooleanDefault))
    }
  }

}
