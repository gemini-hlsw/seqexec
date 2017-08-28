// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package parser

import HorizonsDesignation._

import atto._, Atto._
import cats.implicits._

/** Parser for [[gem.target.HorizonsDesignation]]. */
trait HorizonsDesignationParsers {

  private def des[A](s: String, p: Parser[A]): Parser[A] =
    string(s"${s}_") ~> p

  private def textDes[A](s: String)(f: String => A): Parser[A] =
    des(s, takeText.map(f))

  private def numDes[A](s: String)(f: Int => A): Parser[A] =
    des(s, int.map(f))

  val comet: Parser[Comet] =
    textDes("Comet"      )(Comet.apply      ) namedOpaque "comet"

  val asteroidNew: Parser[AsteroidNew] =
    textDes("AsteroidNew")(AsteroidNew.apply) namedOpaque "asteroidNew"

  val asteroidOld: Parser[AsteroidOld] =
    numDes ("AsteroidOld")(AsteroidOld.apply) namedOpaque "asteroidOld"

  val majorBody: Parser[MajorBody] =
    numDes ("MajorBody"  )(MajorBody.apply  ) namedOpaque "majorBody"

  val horizonsDesignation: Parser[HorizonsDesignation] =
      (comet      .widen[HorizonsDesignation] |
       asteroidNew.widen[HorizonsDesignation] |
       asteroidOld.widen[HorizonsDesignation] |
       majorBody  .widen[HorizonsDesignation] ) named "horizonsDesignation"

}

object HorizonsDesignationParsers extends HorizonsDesignationParsers
