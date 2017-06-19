/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

  // N.B. as of 2.12 we can also us this method to *construct* a decoder by passing a
  // `Node => PioError \/ A)` which conforms with SAM interface PioDecoder[A] (!)
  def apply[A](implicit ev: PioDecoder[A]): PioDecoder[A] = ev

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
