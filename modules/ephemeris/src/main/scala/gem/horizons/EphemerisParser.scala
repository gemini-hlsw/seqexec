// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package horizons

import gem.math.Ephemeris
import gem.util.InstantMicros

import atto.ParseResult
import atto.syntax.parser._

import cats.implicits._

import fs2.Pipe


/** Horizons ephemeris parser.  Parses horizons output generated with the flags
  *
  *   `QUANTITIES=1; time digits=FRACSEC; extra precision=YES`
  *
  * into an `Ephemeris` object.
  */
object EphemerisParser {

  private object impl {

    import atto._
    import Atto._
    import cats.implicits._
    import gem.parser.CoordinateParsers._
    import gem.parser.MiscParsers._
    import gem.parser.TimeParsers._

    val SOE           = "$$SOE"
    val EOE           = "$$EOE"

    val soe           = string(SOE)
    val eoe           = string(EOE)
    val skipPrefix    = manyUntil(anyChar, soe)  ~> verticalWhitespace
    val skipEol       = skipMany(noneOf("\n\r")) ~> verticalWhitespace
    val solarPresence = oneOf("*CNA ").void namedOpaque "solarPresence"
    val lunarPresence = oneOf("mrts ").void namedOpaque "lunarPresence"

    val utc: Parser[InstantMicros] =
      instantUTC(
        genYMD(monthMMM, hyphen) named "yyyy-MMM-dd",
        genLocalTime(colon)
      ).map(InstantMicros.truncate)

    val element: Parser[Ephemeris.Element] =
      for {
        _ <- space
        i <- utc           <~ space
        _ <- solarPresence
        _ <- lunarPresence <~ spaces1
        c <- coordinates
      } yield (i, c)

    val elementLine: Parser[Ephemeris.Element] =
      element <~ skipEol

    val ephemeris: Parser[Ephemeris] =
      skipPrefix ~> (
        many(elementLine).map(Ephemeris.fromFoldable[List]) <~ eoe
      )
  }

  import impl.{ element, ephemeris, SOE, EOE }

  def parse(s: String): ParseResult[Ephemeris] =
    ephemeris.parseOnly(s)

  def elements[F[_]]: Pipe[F, String, Ephemeris.Element] =
    _.through(fs2.text.lines)
     .dropThrough(_.trim =!= SOE)
     .takeWhile(_.trim =!= EOE)
     .map { s => { println(s"'$s'"); element.parseOnly(s).either.left.map(new RuntimeException(_)) } }
     .rethrow

}
