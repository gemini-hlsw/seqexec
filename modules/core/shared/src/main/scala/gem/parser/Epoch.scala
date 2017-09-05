// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.parser

import cats.implicits._
import atto._, Atto._
import gem.math.Epoch

/** Parser for [[gem.math.Epoch]]. */
trait EpochParsers {
  import MiscParsers.intN

  val besselian: Parser[Epoch.Scheme] =
    char('B').as[Epoch.Scheme](Epoch.Besselian) named "besselian"

  val julian: Parser[Epoch.Scheme] =
    char('J').as[Epoch.Scheme](Epoch.Julian) named "julian"

  /** Parser for an `Epoch.Scheme`. */
  val epochScheme: Parser[Epoch.Scheme] =
    (besselian | julian) named "epochScheme"

  /** Parser for an `Epoch`. */
  val epoch: Parser[Epoch] =
    (epochScheme, int <~ char('.'), intN(3)) mapN { (s, y, f) =>
      s.fromMilliyears(y * 1000 + f)
    } named "epoch"

}
object EpochParsers extends EpochParsers
