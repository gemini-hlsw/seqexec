// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicInteger
import org.log4s._
import cats.effect.Timer
import cats.effect.Effect
import cats.effect.Sync
import cats.data.EitherT
import cats.implicits._
import gem.enum.DhsKeywordName
import seqexec.model.dhs.ImageFileId
import seqexec.server.{SeqexecFailure, TrySeq}
import seqexec.server.SeqActionF
import seqexec.server.keywords.DhsClient.ImageParameters
import seqexec.server.SeqexecFailure.SeqexecExceptionWhile
import io.circe.syntax._
import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import org.http4s.client.Client
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.circe._
import org.http4s.client.middleware.{Retry, RetryPolicy}
import org.http4s.client.dsl.Http4sClientDsl
import scala.concurrent.duration._

/**
  * Implementation of DhsClient that interfaces with the real DHS over the http interface
  */
class DhsClientHttp[F[_]: Effect](base: Client[F], baseURI: Uri)(implicit timer: Timer[F]) extends DhsClient[F] with Http4sClientDsl[F] {
  import DhsClientHttp._

  @SuppressWarnings(Array("org.wartremover.warts.Var"))
  private val client = {
    val max = 2
    var attemptsCounter = 1 // scalastyle:ignore
    val policy = RetryPolicy[F] { attempts: Int =>
      if (attempts >= max) None
      else {
        attemptsCounter = attemptsCounter + 1
        Some(10.milliseconds)
      }
    }
    Retry[F](policy)(base)
  }

  override def createImage(p: ImageParameters): F[ImageFileId] = {
    val req = POST(
      Json.obj("createImage" := p.asJson),
      baseURI
    )
    client.expect[TrySeq[ImageFileId]](req)(jsonOf[F, TrySeq[ImageFileId]])
      .attemptT
      .leftMap(SeqexecExceptionWhile("creating image in DHS", _): SeqexecFailure)
      .flatMap(EitherT.fromEither(_)).liftF
  }

  def setParameters(id: ImageFileId, p: ImageParameters): SeqActionF[F, Unit] = {
    val req = PUT(
      Json.obj("setParameters" := p.asJson),
      baseURI / id
    )
    client.expect[TrySeq[Unit]](req)(jsonOf[F, TrySeq[Unit]])
      .attemptT
      .leftMap(SeqexecExceptionWhile("setting image parameters in DHS", _))
      .flatMap(EitherT.fromEither(_))
  }

  override def setKeywords(
    id: ImageFileId,
    keywords: KeywordBag,
    finalFlag: Boolean
  ): F[Unit] = {
    val req = PUT(
      Json.obj("setKeywords" :=
        Json.obj(
          "final" := finalFlag,
          "keywords" := keywords.keywords
        )
      ),
      baseURI / id / "keywords"
    )
    client.expect[TrySeq[Unit]](req)(jsonOf[F, TrySeq[Unit]])
      .attemptT
      .leftMap(SeqexecExceptionWhile("sending keywords to DHS", _))
      .flatMap(EitherT.fromEither(_)).liftF
  }

}

object DhsClientHttp {

  sealed case class ErrorType(str: String)
  object BadRequest extends ErrorType("BAD_REQUEST")
  object DhsError extends ErrorType("DHS_ERROR")
  object InternalServerError extends ErrorType("INTERNAL_SERVER_ERROR")

  implicit def errorTypeDecode: Decoder[ErrorType] = Decoder.instance[ErrorType](c =>  c
    .as[String]
    .map {
      case BadRequest.str          => BadRequest
      case DhsError.str            => DhsError
      case InternalServerError.str => InternalServerError
      case _                       => InternalServerError
    }
  )

  implicit def errorDecode: Decoder[Error] = Decoder.instance[Error]( c =>
    for {
      t   <- c.downField("type").as[ErrorType]
      msg <- c.downField("message").as[String]
    } yield Error(t, msg)
  )

  implicit def obsIdDecode: Decoder[TrySeq[ImageFileId]] = Decoder.instance[TrySeq[ImageFileId]](
    c => {
      val r = c.downField("response")
      val s = r.downField("status").as[String]
      s.flatMap {
        case "success" =>
          r.downField("result").as[String].map(TrySeq(_))
        case "error"   =>
          r.downField("errors").as[List[Error]].map(l =>
            TrySeq.fail[ImageFileId](SeqexecFailure.Unexpected(l.mkString(", ")))
          )
        case r         =>
          Left(DecodingFailure(s"Unknown response: $r", c.history))
      }
    } )

  implicit def unitDecode: Decoder[TrySeq[Unit]] = Decoder.instance[TrySeq[Unit]]( c => {
    val r = c.downField("response")
    val s = r.downField("status").as[String]
    s flatMap {
      case "success" =>
        Right(TrySeq(()))
      case "error"   =>
        r.downField("errors").as[List[Error]].map(
          l => TrySeq.fail[Unit](SeqexecFailure.Unexpected(l.mkString(", "))))
      case r         =>
        Left(DecodingFailure(s"Unknown response: $r", c.history))
    }
  } )

  implicit def imageParametersEncode: Encoder[DhsClient.ImageParameters] =
    Encoder.instance[DhsClient.ImageParameters](p => Json.obj(
      "lifetime" := p.lifetime.str,
      "contributors" := p.contributors
    ))

  implicit def keywordEncode: Encoder[InternalKeyword] = Encoder.instance[InternalKeyword]( k =>
    Json.obj(
      "name" := DhsKeywordName.all.find(_.keyword === k.name).map(_.name).getOrElse(k.name.name),
      "type" := KeywordType.dhsKeywordType(k.keywordType),
      "value" := k.value
    )
  )

  final case class Error(t: ErrorType, msg: String) {
    override def toString = s"(${t.str}) $msg"
  }

  def apply[F[_]: Effect](client: Client[F], uri: Uri)(implicit timer: Timer[F]): DhsClient[F] = new DhsClientHttp[F](client, uri)
}

/**
  * Implementation of the Dhs client that simulates a dhs without external dependencies
  */
class DhsClientSim[F[_]: Sync](date: LocalDate) extends DhsClient[F] {
  import DhsClientSim.Log
  private val counter = new AtomicInteger(0)

  val format: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")

  override def createImage(p: ImageParameters): F[ImageFileId] =
    Sync[F].delay (
      f"S${date.format(format)}S${counter.incrementAndGet()}%04d"
    )

  override def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean): F[Unit] =
    Sync[F].delay(
      Log.info(keywords.keywords.map(k => s"${k.name} = ${k.value}").mkString(", "))
    )

}

object DhsClientSim {
  private val Log = getLogger
  def apply[F[_]: Sync](date: LocalDate): DhsClient[F] = new DhsClientSim[F](date)
}
