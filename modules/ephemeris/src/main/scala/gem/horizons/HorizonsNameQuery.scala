// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.EphemerisKey

import HorizonsNameQuery._

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._

import scala.util.Either
import scala.util.matching.Regex


/** Horizons name-resolution query.
  *
  * {{{
  *   import HorizonsNameQuery.Search.Comet
  *
  *   HorizonsNameQuery(Comet("Halley")).lookup.value.unsafeRunSync
  * }}}
  */
sealed trait HorizonsNameQuery[A] {

  /** URL string corresponding to the horizons request. */
  def urlString: String

  /** Returns a program that will perform the name lookup when executed,
    * returing a List of ephemeris keys with their common name.
    */
  def lookup: Result[List[Resolution[A]]]
}

object HorizonsNameQuery {

  /** Describes a non-sidereal target query of a specific type of object. */
  sealed abstract class Search[A](val queryString: String) extends Product with Serializable

  object Search {
    final case class Comet(partial: String)     extends Search[EphemerisKey.Comet    ](s"NAME=$partial*;CAP")
    final case class Asteroid(partial: String)  extends Search[EphemerisKey.Asteroid ](s"ASTNAM=$partial*"  )
    final case class MajorBody(partial: String) extends Search[EphemerisKey.MajorBody](s"$partial"          )
  }

  sealed trait Error extends Product with Serializable

  final case class HorizonsException(e: Throwable)                  extends Error
  final case class ParseError(input: List[String], message: String) extends Error

  /** Result of performing an horizons name resolution. */
  type Result[A] = EitherT[IO, Error, A]

  /** A human-readable name and its unique horizons ephemeris key. */
  final case class Resolution[A](a: A, name: String)

  /** Creates a NameQuery for the given search object. */
  def apply[A](search: Search[A]): HorizonsNameQuery[A] =
    new HorizonsNameQuery[A] {
      import NameQueryImpl._

      val reqParams = HorizonsClient.SharedParams ++ Map(
        "MAKE_EPHEM" -> "NO",
        "COMMAND"    -> s"'${search.queryString}'"
      )

      override val urlString: String =
        HorizonsClient.urlString(reqParams)

      override val lookup: Result[List[Resolution[A]]] =
        EitherT(HorizonsClient.fetch(reqParams).map { s =>
          val lines = s.split('\n').toList
          parseResponse(search, lines).leftMap(ParseError(lines, _))
        })
    }

  private object NameQueryImpl {

    // Representation of column offsets as (start, end) pairs, as deduced
    // from --- -------- --- -----
    type Offsets = List[(Int, Int)]

    // Parse many results based on an expected header pattern. This will read
    // through the tail until --- separators are found, then parse the
    // remaining lines (until a blank line is encountered) using the function
    // constructed by `f`, if any (otherwise it is assumed that there were not
    // enough columns found). Returns a list of values or an error message.
    def parseMany[A](header: String, tail: List[String], headerPattern: Regex)(f: Offsets => Option[String => A]): Either[String, List[A]] =
      headerPattern.findFirstMatchIn(header).toRight("Header pattern not found: " + headerPattern.regex) *>
        (tail.dropWhile(s => !s.trim.startsWith("---")) match {
          case Nil                => Nil.asRight // no column headers means no results!
          case colHeaders :: rows =>             // we have a header row with data rows following
            val offsets = "-+".r.findAllMatchIn(colHeaders).map(m => (m.start, m.end)).toList
            try {
              f(offsets).map(g => rows.takeWhile(_.trim.nonEmpty).map(g)).toRight("Not enough columns.")
            } catch {
              case    nfe: NumberFormatException           =>  ("Number format exception: " + nfe.getMessage).asLeft
              case      _: StringIndexOutOfBoundsException =>   "Column value(s) not found.".asLeft
            }
        })


    def parseHeader[A](lines: List[String])(f: (String, List[String]) => Either[String, List[A]]): Either[String, List[A]] =
      lines match {
        case _ :: h :: t => f(h, t)
        case _           => "Fewer than 2 lines!".asLeft
      }

    def parseResponse[A](s: Search[A], lines: List[String]): Either[String, List[Resolution[A]]] =
      parseHeader[Resolution[A]](lines) { case (header, tail) =>
        s match {
          case Search.Comet(_)     => parseComets(header, tail)
          case Search.Asteroid(_)  => parseAsteroids(header, tail)
          case Search.MajorBody(_) => parseMajorBodies(header, tail)
        }
      }

    type ParsedResolutions[A] = Either[String, List[Resolution[A]]]
    type ParsedComets         = ParsedResolutions[EphemerisKey.Comet]
    type ParsedAsteroids      = ParsedResolutions[EphemerisKey.Asteroid]
    type ParsedMajorBodies    = ParsedResolutions[EphemerisKey.MajorBody]

    def parseComets(header: String, tail: List[String]): ParsedComets = {

      // Common case is that we have many results, or none.
      def case0: ParsedComets =
        parseMany[Resolution[EphemerisKey.Comet]](header, tail, """  +Small-body Index Search Results  """.r) { offs =>
          (offs.lift(2), offs.lift(3)).mapN {
            case ((ods, ode), (ons, _)) => { row =>
              val desig = row.substring(ods, ode).trim
              val name  = row.substring(ons     ).trim // last column, so no end index because rows are ragged
              Resolution(EphemerisKey.Comet(desig), name)
            }
          }
        }

      // Single result with form: JPL/HORIZONS      Hubble (C/1937 P1)     2015-Dec-31 11:40:21
      def case1: ParsedComets =
        """  +([^(]+)\s+\((.+?)\)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.Comet(m.group(2)), m.group(1)))
        }.toRight("Could not match 'Hubble (C/1937 P1)' header pattern.")

      // Single result with form: JPL/HORIZONS         1P/Halley           2015-Dec-31 11:40:21
      def case2: ParsedComets =
        """  +([^/]+)/(.+?)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.Comet(m.group(1)), m.group(2)))
        }.toRight("Could not match '1P/Halley' header pattern.")

      // First one that works!
      case0 orElse
      case1 orElse
      case2 orElse "Could not parse the header line as a comet".asLeft
    }

    def parseAsteroids(header: String, tail: List[String]): ParsedAsteroids = {

      // Common case is that we have many results, or none.
      def case0: ParsedAsteroids =
        parseMany[Resolution[EphemerisKey.Asteroid]](header, tail, """  +Small-body Index Search Results  """.r) { offs =>
          (offs.lift(0), offs.lift(1), offs.lift(2)).mapN {
            case ((ors, ore), (ods, ode), (ons, _)) => { row =>
              val rec   = row.substring(ors, ore).trim.toInt
              val desig = row.substring(ods, ode).trim
              val name  = row.substring(ons     ).trim // last column, so no end index because rows are ragged
              desig match {
                case "(undefined)" => Resolution(EphemerisKey.AsteroidOld(rec): EphemerisKey.Asteroid, name)
                case des           => Resolution(EphemerisKey.AsteroidNew(des): EphemerisKey.Asteroid, name)
              }
            }
          }
        }

      // Single result with form: JPL/HORIZONS      90377 Sedna (2003 VB12)     2015-Dec-31 11:40:21
      def case1: ParsedAsteroids =
        """  +\d+ ([^(]+)\s+\((.+?)\)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.AsteroidNew(m.group(2)) : EphemerisKey.Asteroid, m.group(1)))
        }.toRight("Could not match '90377 Sedna (2003 VB12)' header pattern.")

      // Single result with form: JPL/HORIZONS      4 Vesta     2015-Dec-31 11:40:21
      def case2: ParsedAsteroids =
        """  +(\d+) ([^(]+?)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.AsteroidOld(m.group(1).toInt) : EphemerisKey.Asteroid, m.group(2)))
        }.toRight("Could not match '4 Vesta' header pattern.")

      // Single result with form: JPL/HORIZONS    (2016 GB222)    2016-Apr-20 15:22:36
      def case3: ParsedAsteroids =
        """  +\((.+?)\)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.AsteroidNew(m.group(1)) : EphemerisKey.Asteroid, m.group(1)))
        }.toRight("Could not match '(2016 GB222)' header pattern.")

      // Single result with form: JPL/HORIZONS        418993 (2009 MS9)            2016-Sep-07 18:23:54
      def case4: ParsedAsteroids =
        """  +\d+\s+\((.+?)\)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.AsteroidNew(m.group(1)) : EphemerisKey.Asteroid, m.group(1)))
        }.toRight("Could not match '418993 (2009 MS9)' header pattern.")

      // Single result with form: JPL/HORIZONS              1I/'Oumuamua (A/2017 U1)         2018-Apr-16 18:28:59
      def case5: ParsedAsteroids =
        """  +\S+\s+\((A/.+?)\)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.AsteroidNew(m.group(1)) : EphemerisKey.Asteroid, m.group(1)))
        }.toRight("Could not match '1I/'Oumuamua (A/2017 U1)' header pattern.")

      // Single result with form: JPL/HORIZONS     A/2017 U7     2015-Dec-31 11:40:21
      def case6: ParsedAsteroids =
        """  +(A/\d+ [^(]+?)  """.r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.AsteroidNew(m.group(1)) : EphemerisKey.Asteroid, m.group(1)))
        }.toRight("Could not match 'A/2017 U7' header pattern.")

      // First one that works!
      case0 orElse
      case1 orElse
      case2 orElse
      case3 orElse
      case4 orElse
      case5 orElse
      case6 orElse "Could not parse the header line as an asteroid".asLeft
    }

    def parseMajorBodies(header: String, tail: List[String]): ParsedMajorBodies = {

      // Common case is that we have many results, or none.
      def case0: ParsedMajorBodies =
        parseMany[Resolution[EphemerisKey.MajorBody]](header, tail, """Multiple major-bodies match string""".r) { offs =>
          (offs.lift(0), offs.lift(1)).mapN {
            case ((ors, ore), (ons, one)) => { row =>
              val rec  = row.substring(ors, ore).trim.toInt
              val name = row.substring(ons, one).trim
              Resolution(EphemerisKey.MajorBody(rec.toInt), name)
            }
          }
        }.map(_.filterNot(_.a.num < 0)) // filter out spacecraft

      // Single result with form:  Revised: Aug 11, 2015       Charon / (Pluto)     901
      def case1: ParsedMajorBodies =
        """  +(.*?) / \((.+?)\)  +(\d+) *$""".r.findFirstMatchIn(header).map { m =>
          List(Resolution(EphemerisKey.MajorBody(m.group(3).toInt), m.group(1)))
        }.toRight("Could not match 'Charon / (Pluto)     901' header pattern.")

      // First one that works, otherwise Nil because it falls through to small-body search
      case0 orElse
      case1 orElse Nil.asRight
    }
  }

}
