package edu.gemini.seqexec.server

import java.util.logging.Logger

import argonaut._
import Argonaut._
import org.apache.commons.httpclient.{HttpMethod, HttpClient}
import org.apache.commons.httpclient.methods.{EntityEnclosingMethod, PutMethod, PostMethod}

import scala.io.Source

import scalaz.concurrent.Task
import scalaz.EitherT

/**
 * Created by jluhrs on 11/5/15.
 */
object DhsClient {

  val baseURI = "http://cpodhsxx:9090/axis2/services/dhs/images"

  type ObsId = String

  sealed case class ErrorType(str: String)
  object BadRequest extends ErrorType("BAD_REQUEST")
  object DhsError extends ErrorType("DHS_ERROR")
  object InternalServerError extends ErrorType("INTERNAL_SERVER_ERROR")

  implicit def errorTypeDecode: DecodeJson[ErrorType] = DecodeJson[ErrorType]( c =>  c.as[String].map {
      case BadRequest.str          => BadRequest
      case DhsError.str            => DhsError
      case InternalServerError.str => InternalServerError
    }
  )

  final case class Error(t: ErrorType, msg: String) {
    override def toString = "(" + t.str + ") " + msg
  }

  implicit def errorDecode: DecodeJson[Error] = DecodeJson[Error]( c => for {
      t <- (c --\ "type").as[ErrorType]
      msg <- (c --\ "message").as[String]
    } yield Error(t, msg)
  )

  implicit def obsIdDecode: DecodeJson[TrySeq[ObsId]] = DecodeJson[TrySeq[ObsId]]( c => {
    val r = c --\ "response"
    val s = (r --\ "status").as[String]
    s flatMap {
      case "success" => (r --\ "result").as[String].map(TrySeq(_))
      case "error"   => (r --\ "errors").as[List[Error]].map(
        l => TrySeq.fail[ObsId](SeqexecFailure.Unexpected(l.mkString(", "))))
    }
  } )

  implicit def unitDecode: DecodeJson[TrySeq[Unit]] = DecodeJson[TrySeq[Unit]]( c => {
    val r = c --\ "response"
    val s = (r --\ "status").as[String]
    s flatMap {
      case "success" => DecodeResult.ok(TrySeq(()))
      case "error"   => (r --\ "errors").as[List[Error]].map(
        l => TrySeq.fail[Unit](SeqexecFailure.Unexpected(l.mkString(", "))))
    }
  } )

  type Contributor = String

  sealed case class Lifetime(str: String)
  object Permanent extends Lifetime("PERMANENT")
  object Temporary extends Lifetime("TEMPORARY")
  object Transient extends Lifetime("TRANSIENT")

  final case class ImageParameters(lifetime: Lifetime, contributors: List[Contributor])

  implicit def imageParametersEncode: EncodeJson[ImageParameters] = EncodeJson[ImageParameters]( p =>
    ("lifetime" := p.lifetime.str) ->: ("contributors" := p.contributors) ->: Json.jEmptyObject )

  // TODO: Implement the unsigned types, if needed.
  sealed case class KeywordType protected (str: String)
  object TypeInt8 extends KeywordType("INT8")
  object TypeInt16 extends KeywordType("INT16")
  object TypeInt32 extends KeywordType("INT32")
  object TypeFloat extends KeywordType("FLOAT")
  object TypeDouble extends KeywordType("DOUBLE")
  object TypeBoolean extends KeywordType("BOOLEAN")
  object TypeString extends KeywordType("STRING")


  // The developer uses these classes to define all the typed keywords
  sealed class Keyword[T] protected (val n: String, val t: KeywordType, val v: T)
  final case class Int8Keyword(name: String, value: Byte) extends Keyword[Byte](name, TypeInt8, value)
  final case class Int16Keyword(name: String, value: Short) extends Keyword[Short](name, TypeInt16, value)
  final case class Int32Keyword(name: String, value: Int) extends Keyword[Int](name, TypeInt32, value)
  final case class FloatKeyword(name: String, value: Float) extends Keyword[Float](name, TypeFloat, value)
  final case class DoubleKeyword(name: String, value: Double) extends Keyword[Double](name, TypeDouble, value)
  final case class BooleanKeyword(name: String, value: Boolean) extends Keyword[Boolean](name, TypeBoolean, value)
  final case class StringKeyword(name: String, value: String) extends Keyword[String](name, TypeString, value)

  // At the end, I want to just pass a list of keywords to be sent to the DHS. I cannot do this with Keyword[T],
  // because I cannot mix different types in a List. But at the end I only care about the value as a String, so I
  // use an internal representation, and offer a class to the developer (KeywordBag) to create the list from typed
  // keywords.

  final protected case class InternalKeyword(name: String, keywordType: KeywordType, value: String)

  protected implicit def internalKeywordConvert[T](k: Keyword[T]): InternalKeyword = InternalKeyword(k.n, k.t, k.v.toString)

  final case class KeywordBag(keywords: List[InternalKeyword]) {
    def add[T](k: Keyword[T]): KeywordBag = KeywordBag(internalKeywordConvert(k) :: keywords)
    def append(other: KeywordBag): KeywordBag =  KeywordBag(keywords ::: other.keywords)
  }

  //TODO: Add more apply methods if necessary
  object KeywordBag {
    def apply: KeywordBag = KeywordBag(List())
    def apply[A](k1: Keyword[A]): KeywordBag = KeywordBag(List(internalKeywordConvert(k1)))
    def apply[A, B](k1: Keyword[A], k2: Keyword[B]): KeywordBag = KeywordBag(List(internalKeywordConvert(k1), internalKeywordConvert(k2)))
    def apply[A, B, C](k1: Keyword[A], k2: Keyword[B], k3: Keyword[C]): KeywordBag =
      KeywordBag(List(internalKeywordConvert(k1), internalKeywordConvert(k2), internalKeywordConvert(k3)))
    def apply[A, B, C, D](k1: Keyword[A], k2: Keyword[B], k3: Keyword[C], k4: Keyword[D]): KeywordBag =
      KeywordBag(List(internalKeywordConvert(k1), internalKeywordConvert(k2), internalKeywordConvert(k3),
        internalKeywordConvert(k4)))
    def apply[A, B, C, D, E](k1: Keyword[A], k2: Keyword[B], k3: Keyword[C], k4: Keyword[D], k5: Keyword[E]): KeywordBag =
      KeywordBag(List(internalKeywordConvert(k1), internalKeywordConvert(k2), internalKeywordConvert(k3),
        internalKeywordConvert(k4), internalKeywordConvert(k5)))
    def apply[A, B, C, D, E, F](k1: Keyword[A], k2: Keyword[B], k3: Keyword[C], k4: Keyword[D], k5: Keyword[E], k6: Keyword[F]): KeywordBag =
      KeywordBag(List(internalKeywordConvert(k1), internalKeywordConvert(k2), internalKeywordConvert(k3),
        internalKeywordConvert(k4), internalKeywordConvert(k5), internalKeywordConvert(k6)))
  }

  implicit def keywordEncode: EncodeJson[InternalKeyword] = EncodeJson[InternalKeyword]( k =>
    ("name" := k.name) ->: ("type" := k.keywordType.str) ->: ("value" := k.value) ->: Json.jEmptyObject )

  private def sendRequest[T](method: EntityEnclosingMethod, body: Json, errMsg: String)(implicit decoder: argonaut.DecodeJson[TrySeq[T]]): SeqAction[T] = EitherT ( Task.delay {
      val client = new HttpClient()

      method.addRequestHeader("Content-Type", "application/json")
      method.setRequestBody(body.nospaces)

      client.executeMethod(method)

      val r = Source.fromInputStream(method.getResponseBodyAsStream).getLines().mkString.decodeOption[TrySeq[T]](decoder)

      method.releaseConnection()

      r.getOrElse(TrySeq.fail[T](SeqexecFailure.Execution(errMsg)))
    } )

  private def createImage(reqBody: Json): SeqAction[ObsId] =
    sendRequest[ObsId](new PostMethod(baseURI), Json.jSingleObject("createImage", reqBody), "Unable to get label")

  def createImage: SeqAction[ObsId] = createImage(Json.jEmptyObject)

  def createImage(p: ImageParameters): SeqAction[ObsId] = createImage(p.asJson)

  def setParameters(id: ObsId, p: ImageParameters): SeqAction[Unit] =
    sendRequest[Unit](new PutMethod(baseURI + "/" + id), Json.jSingleObject("setParameters", p.asJson), "Unable to set parameters for image " + id)

  def setKeywords(id: ObsId, keywords: KeywordBag, finalFlag: Boolean = false): SeqAction[Unit] =
    sendRequest[Unit](new PutMethod(baseURI + "/" + id + "/keywords"),
      Json.jSingleObject("setKeywords", ("final" := finalFlag) ->: ("keywords" := keywords.keywords) ->: Json.jEmptyObject ),
      "Unable to write keywords for image " + id)

}
