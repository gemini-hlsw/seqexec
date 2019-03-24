// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ocs2.pio

import cats.Functor
import java.time.Instant
import shapeless.Typeable
import scala.xml.Node

import PioError._

/** Typeclass for decoders of PIO-ish XML exported from an OCS2 database.
  */
trait PioDecoder[A] {
  def decode(n: Node): Either[PioError, A]
}

object PioDecoder {

  // N.B. as of 2.12 we can also us this method to *construct* a decoder by passing a
  // `Node => Either[PioError, A])` which conforms with SAM interface PioDecoder[A] (!)
  def apply[A](implicit ev: PioDecoder[A]): PioDecoder[A] = ev

  def enum[A: Typeable](m: (String, A)*): PioDecoder[A] =
    fromParseFunction[A] { m.toMap.lift }

  def fromParse[A: Typeable](parse: PioParse[A]): PioDecoder[A] =
    fromParseFunction(parse.run)

  def fromParseFunction[A](parse: String => Option[A])(implicit ev: Typeable[A]): PioDecoder[A] =
    new PioDecoder[A] {
      def decode(n: Node): Either[PioError, A] =
        parse(n.text) toRight ParseError(n.text, ev.describe)
    }

  implicit val FunctorPioDecoder: Functor[PioDecoder] =
    new Functor[PioDecoder] {
      def map[A, B](da: PioDecoder[A])(f: A => B): PioDecoder[B] =
        PioDecoder(n => da.decode(n).map(f))
    }

  implicit def decode2[A, B](implicit da: PioDecoder[A], db: PioDecoder[B]): PioDecoder[(A, B)] =
    PioDecoder { n =>
      for {
        a <- da.decode(n)
        b <- db.decode(n)
      } yield (a, b)
    }

  implicit def decode3[A, B, C](implicit da: PioDecoder[A], db: PioDecoder[B], dc: PioDecoder[C]): PioDecoder[(A, B, C)] =
    PioDecoder { n =>
      for {
        a <- da.decode(n)
        b <- db.decode(n)
        c <- dc.decode(n)
      } yield (a, b, c)
    }

  implicit val DoubleDecoder: PioDecoder[Double]   = fromParse(PioParse.double )
  implicit val StringDecoder: PioDecoder[String]   = fromParse(PioParse.string )
  implicit val IntDecoder: PioDecoder[Int]         = fromParse(PioParse.int    )
  implicit val LongDecoder: PioDecoder[Long]       = fromParse(PioParse.long   )
  implicit val InstantDecoder: PioDecoder[Instant] = fromParse(PioParse.instant)
}
