// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package parser

import EphemerisKey._
import atto._
import Atto._
import cats.implicits._

/** Parser for [[gem.EphemerisKey]]. */
trait EphemerisKeyParsers {

  private def des[A](s: String, p: Parser[A]): Parser[A] =
    string(s"${s}_") ~> p

  private def textDes[A](s: String)(f: String => A): Parser[A] =
    des(s, takeText.map(f))

  private def numDes[A](s: String)(f: Int => A): Parser[A] =
    des(s, int.map(f))

  val comet: Parser[Comet] =
    textDes("Comet")(Comet.apply) namedOpaque "comet"

  val asteroidNew: Parser[AsteroidNew] =
    textDes("AsteroidNew")(AsteroidNew.apply) namedOpaque "asteroidNew"

  val asteroidOld: Parser[AsteroidOld] =
    numDes("AsteroidOld")(AsteroidOld.apply) namedOpaque "asteroidOld"

  val majorBody: Parser[MajorBody] =
    numDes("MajorBody")(MajorBody.apply) namedOpaque "majorBody"

  val userSupplied: Parser[UserSupplied] =
    numDes("UserSupplied")(UserSupplied.apply) namedOpaque "userSupplied"

  val ephemerisKey: Parser[EphemerisKey] =
      (comet       .widen[EphemerisKey] |
       asteroidNew .widen[EphemerisKey] |
       asteroidOld .widen[EphemerisKey] |
       majorBody   .widen[EphemerisKey] |
       userSupplied.widen[EphemerisKey]) named "ephemerisKey"

}

object EphemerisKeyParsers extends EphemerisKeyParsers
