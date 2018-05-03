// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.server.http4s.encoder

import java.nio.ByteBuffer

import boopickle.Default._
import boopickle.Pickler
import cats.Applicative
import cats.effect.Sync
import org.http4s._
import org.http4s.EntityEncoder.chunkEncoder
import org.http4s.headers.`Content-Type`
import fs2.Chunk
import scala.util.{Failure, Success}

/**
  * Generic factories for http4s encoders/decoders for boopickle
  * Note that the media type is set for application/octet-stream
  */
trait BooPickleInstances {

  def booOf[F[_]: Sync, A: Pickler]: EntityDecoder[F, A] =
    EntityDecoder.decodeBy(MediaType.`application/octet-stream`)(booDecoderByteBuffer[F, A])

  private def booDecoderByteBuffer[F[_]: Sync, A](msg: Message[F])(implicit pickler: Pickler[A]): DecodeResult[F, A] =
    EntityDecoder.collectBinary(msg).flatMap { segment =>
      val bb = ByteBuffer.wrap(segment.force.toArray)
      if (bb.hasRemaining) {
        Unpickle[A](pickler).tryFromBytes(bb) match {
          case Success(bb) =>
            DecodeResult.success[F, A](bb)
          case Failure(pf) =>
            DecodeResult.failure[F, A](
              MalformedMessageBodyFailure("Invalid binary body", Some(pf)))
        }
      } else {
        DecodeResult.failure[F, A](MalformedMessageBodyFailure("Invalid binary: empty body", None))
      }
    }

  def booEncoderOf[F[_]: Applicative, A: Pickler]: EntityEncoder[F, A] =
    chunkEncoder[F].contramap[A] { v =>
      Chunk.ByteBuffer(Pickle.intoBytes(v))
    }.withContentType(`Content-Type`(MediaType.`application/octet-stream`))

}
