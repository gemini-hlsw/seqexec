package edu.gemini.seqexec.web.server.http4s.encoder

import java.nio.ByteBuffer

import boopickle.Default._
import boopickle.Pickler

import org.http4s._
import org.http4s.headers.`Content-Type`
import scodec.bits.ByteVector

import scalaz.-\/
import scalaz.stream.Process._

/**
  * Generic factories for http4s encoders/decoders for boopickle
  * Note that the media type is set for application/octet-stream
  */
trait BooPickleInstances {

  def booOf[A](implicit pickler: Pickler[A]): EntityDecoder[A] =
    EntityDecoder.decodeBy(MediaType.`application/octet-stream`) { msg =>
      DecodeResult {
        msg.body.map(bv => Unpickle[A](pickler).fromBytes(bv.toByteBuffer)).partialAttempt {
          case e: Exception => emit(MalformedMessageBodyFailure("Invalid binary body", Some(e)))
        }.runLastOr(-\/(MalformedMessageBodyFailure("Invalid binary: empty body")))
      }
    }

  def booEncoderOf[A](implicit encoder: Pickler[A]): EntityEncoder[A] =
    EntityEncoder[ByteVector].contramap[A] { v =>
      ByteVector(Pickle.intoBytes(v))
    }.withContentType(`Content-Type`(MediaType.`application/octet-stream`))
}
