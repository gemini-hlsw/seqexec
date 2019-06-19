// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.test.RespectIncludeTags
import gem.test.Tags._

import gem.{ EphemerisKey => EK }

import cats.tests.CatsSuite

import scala.util.Either

final class HorizonsNameQuerySpec extends CatsSuite with RespectIncludeTags {
  import HorizonsNameQuery._

  def runSearch[A](s: Search[A]): Either[Error, List[Resolution[A]]] =
    HorizonsNameQuery(s).lookup.value.unsafeRunSync

  test("comet search should handle empty results", RequiresNetwork) {
    runSearch(Search.Comet("covfefe")) shouldEqual Nil.asRight
  }

  test("comet search should handle multiple results", RequiresNetwork) {
    runSearch(Search.Comet("hu")).map(_.take(5)) shouldEqual List(
      Resolution(EK.Comet("67P"), "Churyumov-Gerasimenko"),
      Resolution(EK.Comet("106P"), "Schuster"),
      Resolution(EK.Comet("130P"), "McNaught-Hughes"),
      Resolution(EK.Comet("178P"), "Hug-Bell"),
      Resolution(EK.Comet("C/1880 Y1"), "Pechule")
    ).asRight
  }

  test("comet search should handle single result (Format 1) Hubble (C/1937 P1)", RequiresNetwork) {
    runSearch(Search.Comet("hubble")) shouldEqual List(
      Resolution(EK.Comet("C/1937 P1"), "Hubble")
    ).asRight
  }

  test("comet search should handle single result (Format 2) 1P/Halley pattern", RequiresNetwork) {
    runSearch(Search.Comet("halley")) shouldEqual List(
      Resolution(EK.Comet("1P"), "Halley")
    ).asRight
  }

  test("asteroid search should handle empty results", RequiresNetwork) {
    runSearch(Search.Asteroid("covfefe")) shouldEqual Nil.asRight
  }

  test("asteroid search should handle multiple results", RequiresNetwork) {
    runSearch(Search.Asteroid("her")).map(_.take(5)) shouldEqual List(
      Resolution(EK.AsteroidOld(103), "Hera"),
      Resolution(EK.AsteroidOld(121), "Hermione"),
      Resolution(EK.AsteroidOld(135), "Hertha"),
      Resolution(EK.AsteroidOld(206), "Hersilia"),
      Resolution(EK.AsteroidOld(214), "Aschera")
    ).asRight
  }

  test("asteroid search should handle single result (Format 1) 90377 Sedna (2003 VB12)", RequiresNetwork) {
    runSearch(Search.Asteroid("sedna")) shouldEqual List(
      Resolution(EK.AsteroidNew("2003 VB12"), "Sedna")
    ).asRight
  }

  test("asteroid search should handle single result (Format 2) 29 Amphitrite", RequiresNetwork) {
    runSearch(Search.Asteroid("amphitrite")) shouldEqual List(
      Resolution(EK.AsteroidOld(29), "Amphitrite")
    ).asRight
  }

  test("asteroid search should handle single result (Format 3) (2016 GB222)", RequiresNetwork) {
    runSearch(Search.Asteroid("2016 GB222")) shouldEqual List(
      Resolution(EK.AsteroidNew("2016 GB222"), "2016 GB222")
    ).asRight
  }

  test("asteroid search should handle single result (Format 4) 418993 (2009 MS9)", RequiresNetwork) {
    runSearch(Search.Asteroid("2009 MS9")) shouldEqual List(
      Resolution(EK.AsteroidNew("2009 MS9"), "2009 MS9")
    ).asRight
  }

  test("asteroid search should handle single result (Format 5) 1I/'Oumuamua (A/2017 U1)", RequiresNetwork) {
    runSearch(Search.Asteroid("A/2017 U1")) shouldEqual List(
      Resolution(EK.AsteroidNew("A/2017 U1"), "A/2017 U1")
    ).asRight
  }

  test("major body search should handle empty results", RequiresNetwork) {
    runSearch(Search.MajorBody("covfefe")) shouldEqual Nil.asRight
  }

  test("major body search should handle empty results with small-body fallthrough (many)", RequiresNetwork) {
    runSearch(Search.MajorBody("hu")) shouldEqual Nil.asRight
  }

  test("major body search should handle empty results with small-body fallthrough (single)", RequiresNetwork) {
    runSearch(Search.MajorBody("hermione")) shouldEqual Nil.asRight
  }

  test("major body search should handle multiple results", RequiresNetwork) {
    runSearch(Search.MajorBody("mar")).map(_.take(5)) shouldEqual List(
      Resolution(EK.MajorBody(4), "Mars Barycenter"),
      Resolution(EK.MajorBody(499), "Mars"),
      Resolution(EK.MajorBody(723), "Margaret")
    ).asRight
  }

  test("major body search should handle single result with trailing space (!)", RequiresNetwork) {
    runSearch(Search.MajorBody("charon")) shouldEqual List(
      Resolution(EK.MajorBody(901), "Charon")
    ).asRight
  }

  test("major body search should handle single result without trailing space", RequiresNetwork) {
    runSearch(Search.MajorBody("europa")) shouldEqual List(
      Resolution(EK.MajorBody(502), "Europa")
    ).asRight
  }
}
