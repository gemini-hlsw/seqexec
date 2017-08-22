// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.parser

import cats.implicits._
import atto._, Atto._
import gem.math.Epoch

/** Parser for [[gem.Epoch]]. */
trait EpochParsers {
  import MiscParsers.intN

  /** Parser for an `Epoch.Scheme`. */
  val epochScheme: Parser[Epoch.Scheme] =
    char('B').as(Epoch.Besselian) |
    char('J').as(Epoch.Julian)

  /** Parser for an `Epoch`. */
  val epoch: Parser[Epoch] =
    for {
      scheme <- epochScheme
      year   <- int
      _      <- char('.')
      frac   <- intN(3)
    } yield scheme.fromMilliyears(year * 1000 + frac)

}
object EpochParsers extends EpochParsers
