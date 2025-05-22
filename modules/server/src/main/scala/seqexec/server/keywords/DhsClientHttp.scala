// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.FlatMap
import cats.data.EitherT
import cats.effect.Ref
import cats.effect.Sync
import cats.effect.Temporal
import cats.syntax.all._
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware.Retry
import org.http4s.client.middleware.RetryPolicy
import org.http4s.dsl.io._
import org.typelevel.log4cats.Logger
import seqexec.model.dhs._
import seqexec.model.enums.DhsKeywordName
import seqexec.model.enums.KeywordName
import seqexec.server.SeqexecFailure
import seqexec.server.SeqexecFailure.SeqexecExceptionWhile
import seqexec.server.keywords.DhsClient.ImageParameters

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._

/**
 * Implementation of DhsClient that interfaces with the real DHS over the http interface
 */
class DhsClientHttp[F[_]](base: Client[F], baseURI: Uri, maxKeywords: Int, instrumentName: String)(
  implicit timer: Temporal[F]
) extends DhsClient[F]
    with Http4sClientDsl[F] {
  import DhsClientHttp._

  private val clientWithRetry = {
    val max             = 4
    var attemptsCounter = 1
    val policy          = RetryPolicy[F] { attempts: Int =>
      if (attempts >= max) None
      else {
        attemptsCounter = attemptsCounter + 1
        10.milliseconds.some
      }
    }
    Retry[F](policy)(base)
  }

  override def createImage(p: ImageParameters): F[ImageFileId] = {
    val req = POST(
      Json.obj("createImage" := p.asJson),
      baseURI
    )
    clientWithRetry
      .expect[Either[SeqexecFailure, ImageFileId]](req)(
        jsonOf[F, Either[SeqexecFailure, ImageFileId]]
      )
      .attemptT
      .leftMap(SeqexecExceptionWhile("creating image in DHS", _): SeqexecFailure)
      .flatMap(EitherT.fromEither(_))
      .liftF
  }

  private def keywordsPutReq(id: ImageFileId, keywords: List[InternalKeyword], finalFlag: Boolean) =
    PUT(
      Json.obj(
        "setKeywords" :=
          Json.obj(
            "final"    := finalFlag,
            "keywords" := keywords
          )
      ),
      baseURI / id / "keywords"
    )

  override def setKeywords(
    id:        ImageFileId,
    keywords:  KeywordBag,
    finalFlag: Boolean
  ): F[Unit] = {

    val minKeywords: Int = 10

    val limit = minKeywords.max(maxKeywords)

    val pkgs = keywords.keywords.grouped(limit).toList

    if (pkgs.isEmpty) {
      clientWithRetry
        .expect[Either[SeqexecFailure, Unit]](
          keywordsPutReq(
            id,
            List(internalKeywordConvert(StringKeyword(KeywordName.INSTRUMENT, instrumentName))),
            finalFlag
          )
        )(jsonOf[F, Either[SeqexecFailure, Unit]])
        .attemptT
        .leftMap(SeqexecExceptionWhile("sending keywords to DHS", _))
        .flatMap(EitherT.fromEither(_))
        .liftF
        .whenA(finalFlag)
    } else
      pkgs.zipWithIndex
        .map { case (l, i) =>
          clientWithRetry
            .expect[Either[SeqexecFailure, Unit]](
              keywordsPutReq(
                id,
                l.prepended(
                  internalKeywordConvert(StringKeyword(KeywordName.INSTRUMENT, instrumentName))
                ),
                (i === pkgs.length - 1) && finalFlag
              )
            )(jsonOf[F, Either[SeqexecFailure, Unit]])
            .attemptT
            .leftMap(SeqexecExceptionWhile("sending keywords to DHS", _))
            .flatMap(EitherT.fromEither(_))
            .liftF
        }
        .sequence
        .void
  }

}

object DhsClientHttp {

  sealed case class ErrorType(str: String)
  object BadRequest          extends ErrorType("BAD_REQUEST")
  object DhsError            extends ErrorType("DHS_ERROR")
  object InternalServerError extends ErrorType("INTERNAL_SERVER_ERROR")

  implicit def errorTypeDecode: Decoder[ErrorType] = Decoder.instance[ErrorType](c =>
    c
      .as[String]
      .map {
        case BadRequest.str          => BadRequest
        case DhsError.str            => DhsError
        case InternalServerError.str => InternalServerError
        case _                       => InternalServerError
      }
  )

  implicit def errorDecode: Decoder[Error] = Decoder.instance[Error](c =>
    for {
      t   <- c.downField("type").as[ErrorType]
      msg <- c.downField("message").as[String]
    } yield Error(t, msg)
  )

  implicit def obsIdDecode: Decoder[Either[SeqexecFailure, ImageFileId]] =
    Decoder.instance[Either[SeqexecFailure, ImageFileId]] { c =>
      val r = c.downField("response")
      val s = r.downField("status").as[String]
      s.flatMap {
        case "success" =>
          r.downField("result").as[String].map(i => toImageFileId(i).asRight)
        case "error"   =>
          r.downField("errors")
            .as[List[Error]]
            .map(l => SeqexecFailure.Unexpected(l.mkString(", ")).asLeft)
        case r         =>
          Left(DecodingFailure(s"Unknown response: $r", c.history))
      }
    }

  implicit def unitDecode: Decoder[Either[SeqexecFailure, Unit]] =
    Decoder.instance[Either[SeqexecFailure, Unit]] { c =>
      val r = c.downField("response")
      val s = r.downField("status").as[String]
      s.flatMap {
        case "success" =>
          ().asRight.asRight
        case "error"   =>
          r.downField("errors")
            .as[List[Error]]
            .map(l => SeqexecFailure.Unexpected(l.mkString(", ")).asLeft[Unit])
        case r         =>
          Left(DecodingFailure(s"Unknown response: $r", c.history))
      }
    }

  implicit def imageParametersEncode: Encoder[DhsClient.ImageParameters] =
    Encoder.instance[DhsClient.ImageParameters](p =>
      Json.obj(
        "lifetime"     := p.lifetime.str,
        "contributors" := p.contributors
      )
    )

  implicit def keywordEncode: Encoder[InternalKeyword] =
    Encoder.instance[InternalKeyword](k =>
      Json.obj(
        "name"  := DhsKeywordName.all.find(_.keyword === k.name).map(_.name).getOrElse(k.name.name),
        "type"  := KeywordType.dhsKeywordType(k.keywordType),
        "value" := k.value
      )
    )

  final case class Error(t: ErrorType, msg: String) {
    override def toString = s"(${t.str}) $msg"
  }

  def apply[F[_]](client: Client[F], uri: Uri, maxKeywords: Int, instrumentName: String)(implicit
    timer: Temporal[F]
  ): DhsClient[F] =
    new DhsClientHttp[F](client, uri, maxKeywords, instrumentName)
}

/**
 * Implementation of the Dhs client that simulates a dhs without external dependencies
 */
private class DhsClientSim[F[_]: FlatMap: Logger](date: LocalDate, counter: Ref[F, Int])
    extends DhsClient[F] {

  val format: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")

  override def createImage(p: ImageParameters): F[ImageFileId] =
    counter.modify(x => (x + 1, x + 1)).map { c =>
      toImageFileId(f"S${date.format(format)}S$c%04d")
    }

  override def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean): F[Unit] = {
    val keyStr = keywords.keywords.map(k => s"${k.name} = ${k.value}").mkString(", ")
    Logger[F].info(s"file: $id, final: $finalFlag, keywords: $keyStr")
  }

}

object DhsClientSim {
  def apply[F[_]: Sync: Logger]: F[DhsClient[F]] =
    (Sync[F].delay(LocalDate.now), Ref.of[F, Int](0)).mapN(new DhsClientSim[F](_, _))

  def apply[F[_]: Sync: Logger](date: LocalDate): F[DhsClient[F]] =
    Ref.of[F, Int](0).map(new DhsClientSim[F](date, _))

}
