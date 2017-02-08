package gem.ocs2.pio

import PioError._

import java.time.Instant

import shapeless.Typeable
import scala.xml.Node
import scalaz.Scalaz._
import scalaz._

/** Typeclass for decoders of PIO-ish XML exported from an OCS2 database.
  */
trait PioDecoder[A] {
  def decode(n: Node): PioError \/ A
}

object PioDecoder {
  def apply[A](implicit ev: PioDecoder[A]): PioDecoder[A] = ev

  def apply[A](f: Node => PioError \/ A): PioDecoder[A] =
    new PioDecoder[A] {
      def decode(n: Node): PioError \/ A =
        f(n)
    }

  def enum[A: Typeable](m: (String, A)*): PioDecoder[A] =
    fromParseFunction[A] { m.toMap.lift }

  def fromParse[A: Typeable](parse: PioParse[A]): PioDecoder[A] =
    fromParseFunction(parse.run)

  def fromParseFunction[A](parse: String => Option[A])(implicit ev: Typeable[A]): PioDecoder[A] =
    new PioDecoder[A] {
      def decode(n: Node): PioError \/ A =
        parse(n.text) \/> ParseError(n.text, ev.describe)
    }

  implicit val FunctorPioDecoder = new Functor[PioDecoder] {
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

  implicit val StringDecoder: PioDecoder[String]   = fromParse(PioParse.string )
  implicit val IntDecoder: PioDecoder[Int]         = fromParse(PioParse.int    )
  implicit val LongDecoder: PioDecoder[Long]       = fromParse(PioParse.long   )
  implicit val InstantDecoder: PioDecoder[Instant] = fromParse(PioParse.instant)
}
