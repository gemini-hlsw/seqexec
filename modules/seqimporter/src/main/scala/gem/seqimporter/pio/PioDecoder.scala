package gem.seqimporter.pio

import PioError._

import java.time.Instant

import scala.xml.Node
import scalaz.Scalaz._
import scalaz._

/** Typeclass for decoders of PIO-ish XML exported from an OCS2 database.
  */
trait PioDecoder[A] { outer =>
  def decode(n: Node): PioError \/ A
}

object PioDecoder {
  def apply[A](implicit ev: PioDecoder[A]): PioDecoder[A] = ev

  def apply[A](f: Node => PioError \/ A): PioDecoder[A] =
    new PioDecoder[A] {
      def decode(n: Node): PioError \/ A =
        f(n)
    }

  def enum[A](dataType: String)(m: (String, A)*): PioDecoder[A] =
    fromParse[A](dataType) { m.toMap.lift }

  def fromParse[A](dataType: String)(parse: PioParse[A]): PioDecoder[A] =
    new PioDecoder[A] {
      def decode(n: Node): PioError \/ A =
        parse(n.text) \/> ParseError(n.text, dataType)
    }

  implicit val StringDecoder: PioDecoder[String]   = fromParse("String" )(PioParse.string )
  implicit val IntDecoder: PioDecoder[Int]         = fromParse("Int"    )(PioParse.int    )
  implicit val LongDecoder: PioDecoder[Long]       = fromParse("Long"   )(PioParse.long   )
  implicit val InstantDecoder: PioDecoder[Instant] = fromParse("Instant")(PioParse.instant)
}
