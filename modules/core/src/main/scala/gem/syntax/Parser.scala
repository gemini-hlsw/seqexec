// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

import atto._, Atto._

final class ParserOps[A](val self: Parser[A]) extends AnyVal {

  /** Parse entire input into an Option. */
  def parseExact(s: String): Option[A] =
    phrase(self).parseOnly(s).option

  /** Parse into an Option, discarding unused input. */
  def parse(s: String): Option[A] =
    self.parseOnly(s).option

}

trait ToParserOps {
  implicit def ToParserOps[A](p: Parser[A]): ParserOps[A] = new ParserOps[A](p)
}

object parser extends ToParserOps
