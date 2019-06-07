// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package horizons

import gem.math.{ Ephemeris, EphemerisCoordinates }
import gem.util.Timestamp
import gsp.math.{ Angle, Offset }

import atto.ParseResult
import atto.ParseResult.Done
import atto.syntax.parser._

import cats.ApplicativeError
import cats.implicits._

import fs2.Pipe

import scala.math.BigDecimal.RoundingMode.HALF_EVEN


/** Horizons ephemeris parser.  Parses horizons output generated with the flags
  *
  *   `QUANTITIES=1; time digits=FRACSEC; extra precision=YES`
  *
  * into an `Ephemeris` object, or a `Stream[Ephemeris.Element]`.
  */
object EphemerisParser {

  private object impl {

    import atto._
    import Atto._
    import cats.implicits._
    import gsp.math.parser.CoordinateParsers._
    import gsp.math.parser.MiscParsers._
    import gsp.math.parser.TimeParsers._

    val SOE           = "$$SOE"
    val EOE           = "$$EOE"

    val soe           = string(SOE)
    val eoe           = string(EOE)
    val skipPrefix    = manyUntil(anyChar, soe)  ~> verticalWhitespace
    val skipEol       = skipMany(noneOf("\n\r")) ~> verticalWhitespace
    val solarPresence = oneOf("*CNA ").void namedOpaque "solarPresence"
    val lunarPresence = oneOf("mrts ").void namedOpaque "lunarPresence"

    val deltaAngle    = bigDecimal.map { d =>
      val µas = d.underlying.movePointRight(6).setScale(0, HALF_EVEN).longValue
      Angle.fromMicroarcseconds(µas)
    }

    val utc: Parser[Timestamp] =
      instantUTC(
        genYMD(monthMMM, hyphen) named "yyyy-MMM-dd",
        genLocalTime(colon)
      ).flatMap(Timestamp.fromInstant(_).fold(err[Timestamp]("date out of range"))(ok))

    val element: Parser[Ephemeris.Element] =
      for {
        _ <- space
        i <- utc           <~ space
        _ <- solarPresence
        _ <- lunarPresence <~ spaces1
        c <- coordinates   <~ spaces1
        p <- deltaAngle    <~ spaces1
        q <- deltaAngle
      } yield (i, EphemerisCoordinates(c, Offset(Offset.P(p), Offset.Q(q))))

    val elementLine: Parser[Ephemeris.Element] =
      element <~ skipEol

    val ephemeris: Parser[Ephemeris] =
      skipPrefix ~> (
        many(elementLine).map(Ephemeris.fromFoldable[List]) <~ eoe
      )
  }

  import impl.{ element, ephemeris, SOE, EOE }

  /** Parses an ephemeris file into an `Ephemeris` object in memory.
    *
    * @param s string containing the ephemeris data from horizons
    *
    * @return result of parsing the string into an `Ephemeris` object
    */
  def parse(s: String): ParseResult[Ephemeris] =
    ephemeris.parseOnly(s)

  /** An `fs2.Pipe` that converts a `Stream[F, String]` of ephemeris data from
    * horizons into a `Stream[F, ParseResult[Ephemeris.Element]]`.
    *
    * @tparam F effect to use
    *
    * @return pipe for a `Stream[F, String]` into a `Stream[F, ParseResult[Ephemeris.Element]]`
    */
  def parsedElements[F[_]]: Pipe[F, String, ParseResult[Ephemeris.Element]] =
    _.through(fs2.text.lines)
     .dropWhile(_.trim =!= SOE)
     .drop(1)
     .takeWhile(_.trim =!= EOE)
     .map(element.parseOnly)

  /** An `fs2.Pipe` that converts a `Stream[F, String]` of ephemeris data from
    * horizons into a `Stream[F, Either[String, Ephemeris.Element]]` where left
    * values indicate parsing errors.
    *
    * @tparam F effect to use
    *
    * @return pipe for a `Stream[F, String]` into a `Stream[F, Either[String, Ephemeris.Element]]`
    */
  def eitherElements[F[_]]: Pipe[F, String, Either[String, Ephemeris.Element]] =
    _.through(parsedElements)
     .map(_.either)

  /** An `fs2.Pipe` that converts a `Stream[F, String]` of ephemeris data from
    * horizons into a `Stream[F, Ephemeris.Element]`.  If there is a parse
    * error reading the data, the element that does not parse is skipped.
    *
    * @tparam F effect to use
    *
    * @return pipe for a `Stream[F, String]` into a `Stream[F, Ephemeris.Element]`
    */
  def validElements[F[_]]: Pipe[F, String, Ephemeris.Element] =
    _.through(parsedElements)
     .collect { case Done(_, e) => e }

  /** An `fs2.Pipe` that converts a `Stream[F, String]` of ephemeris data from
    * horizons into a `Stream[F, Ephemeris.Element]`.  If there is a parse
    * error reading the data, the Stream raises an error.  See `Stream.onError`
    * to handle this case.
    *
    * @tparam F effect to use
    *
    * @return pipe for a `Stream[F, String]` into a `Stream[F, Ephemeris.Element]`
    */
  def elements[F[_]: ApplicativeError[?[_], Throwable]]: Pipe[F, String, Ephemeris.Element] =
    _.through(parsedElements)
     .map(_.either.left.map(new RuntimeException(_)))
     .rethrow
}
