package edu.gemini.seqexec.web.server.http4s.encoder

import java.nio.ByteBuffer

import boopickle.Default._
import boopickle.Pickler

import org.http4s._
import org.http4s.headers.`Content-Type`

import scalaz.-\/
import scalaz.stream.Process._

trait BooPickleInstances {

  private def boopickleDecoder[A](implicit pickler: Pickler[A]): EntityDecoder[A] =
    EntityDecoder.decodeBy(MediaType.`application/octet-stream`) { msg =>
      DecodeResult {
        msg.body.map(bv => Unpickle[A](pickler).fromBytes(bv.toByteBuffer)).partialAttempt {
          case e: Exception => emit(MalformedMessageBodyFailure("Invalid binary body", Some(e)))
        }.runLastOr(-\/(MalformedMessageBodyFailure("Invalid binary: empty body")))
      }
    }

  def booOf[A](implicit pickler: Pickler[A]): EntityDecoder[A] = boopickleDecoder

  def booEncoderOf[A](implicit encoder: Pickler[A]): EntityEncoder[A] =
    EntityEncoder[ByteBuffer].contramap[A] { v =>
      Pickle.intoBytes(v)
    }.withContentType(`Content-Type`(MediaType.`application/octet-stream`))
}

