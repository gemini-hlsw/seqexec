package edu.gemini.seqexec.web.server.http4s.encoder

import boopickle.Default._
import boopickle.Pickler
import org.http4s.{MediaRange, _}
import org.http4s.MediaRange._
import org.http4s.headers.`Content-Type`
import scodec.bits.ByteVector

import scalaz.EitherT
import scalaz.concurrent.Task

/**
  * Created by cquiroz on 6/20/16.
  */
class BooPickleInstances {
  implicit def boopickleDecoder[A]: EntityDecoder[A] = EntityDecoder.decodeBy(MediaType.`application/octet-stream`) { msg =>
    val d = implicitly[Pickler[A]]
    DecodeResult {
      Task.now(msg.body.map(bv => Unpickle[A].fromBytes(bv.toByteBuffer)))
    }
  }

  def booOf[A](implicit decoder: Pickler[A]): EntityDecoder[A] =
    jsonDecoder.flatMapR { json =>
      decoder.decodeJson(json).fold(
        failure =>
          DecodeResult.failure(InvalidMessageBodyFailure(s"Could not decode JSON: $json", Some(failure))),
        DecodeResult.success(_)
      )
    }

  /*implicit def jsonEncoder[A]: EntityEncoder[A] =
    EntityEncoder[ByteVector].contramap[A] { bytes =>
      // Comment from ArgonautInstances (which this code is based on):
      // TODO naive implementation materializes to a String.
      // See https://github.com/non/jawn/issues/6#issuecomment-65018736
      Printer.noSpaces.pretty(json)
    }.withContentType(`Content-Type`(MediaType.`application/json`))

  def jsonEncoderOf[A](implicit encoder: Encoder[A]): EntityEncoder[A] =
    jsonEncoder.contramap[A](encoder.apply)*/
}

