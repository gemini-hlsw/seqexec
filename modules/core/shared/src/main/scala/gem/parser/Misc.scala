// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.parser

import atto._, Atto._
import scala.annotation.tailrec

/** General-purpose parsers and combinators that aren't provided by atto. */
trait MiscParsers {

  /** Parser that always succeeds without consuming any input. */
  val void: Parser[Unit] =
    ok(()) namedOpaque "void"

  /** Parser for a hyphen. */
  val hyphen: Parser[Unit] =
    char('-').void namedOpaque "hyphen"

  /** Parser for one or more spaces. */
  val spaces1: Parser[Unit] =
    skipMany1(char(' ')) namedOpaque "spaces1"

  /** Parser for a non-whitespace string. */
  val nonWhitespace: Parser[String] =
    takeWhile(c => !c.isWhitespace) namedOpaque "nonWhitespace"

  /** Catch a `NumberFormatException`, useful for flatMap. */
  def catchNFE[A, B](f: A => B): A => Parser[B] = a =>
    try ok(f(a)) catch { case e: NumberFormatException => err[B](e.toString) }

  /** Parser for `n` consecutive digits, parsed as an `Int`. */
  def intN(n:Int): Parser[Int] =
    count(n, digit).map(_.mkString).flatMap(catchNFE(_.toInt)) namedOpaque s"intN($n)"

  /** Parser for a positive `Int`. */
  val positiveInt: Parser[Int] =
    int.filter(_ > 0) namedOpaque "positiveInt"

  /** Parser for an optional sign (+ or -), returned as a boolean indicating whether to negate. */
  val neg: Parser[Boolean] =
    opt(char('-').void || char('+').void).map {
      case Some(Left(()))  => true
      case Some(Right(())) => false
      case None            => false
    } namedOpaque "neg"

  /**
   * Fractional portion of a decimal value, with up to N places given. So frac(3) parsing "12"
   * yields 120. Mind the overflow, this only works for small N.
   */
  def frac(n: Int): Parser[Int] = {

    def nexp(n: Int, e: Int): Int = {
      @tailrec def go(e: Int, acc: Int): Int =
        if (e < 1) acc else go(e - 1, acc * n)
      go(e, 1)
    }

    if (n < 1) ok(0)
    else opt(digit.map(_ - '0')).flatMap {
      case None    => ok(0)
      case Some(d) => frac(n - 1).map(_ + d * nexp(10, n - 1))
    }

  } namedOpaque s"frac($n)"


  /** Force and return all remaining input without consuming it. */
  val force: Parser[String] =
    get.flatMap { s =>
      opt(ensure(s.length + 1)) flatMap {
        case Some(_) => force
        case None    => ok(s)
      }
    }

  /**
   * Force and examine remaining input and yield the longest prefix up to (but not including) a
   * char satisfying `p`, which is discarded. Fails is no such char is found.
   */
  def takeUntilLast(p: Char => Boolean): Parser[String] =
    force.flatMap { s =>
      s.lastIndexWhere(p) match {
        case -1 => err[String]("not satisfied")
        case  n => take(n) <~ advance(1)
      }
    } named "takeUntilLast(…)"

}
object MiscParsers extends MiscParsers
