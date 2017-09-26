// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons

import gem.test.RespectIncludeTags
import gem.test.Tags._

import gem.{ EphemerisKey => EK }

import cats.tests.CatsSuite

import scala.util.Either

@SuppressWarnings(Array("org.wartremover.warts.Equals"))
final class NameQuerySpec extends CatsSuite with RespectIncludeTags {
  import NameQuery._

  def runSearch[A](s: Search[A]): Either[Error, List[Row[A]]] =
    fromSearch(s).results.value.unsafeRunSync

  test("comet search should handle empty results", RequiresNetwork) {
    runSearch(Search.Comet("covfefe")) shouldEqual Nil.asRight
  }

  test("comet search should handle multiple results", RequiresNetwork) {
    runSearch(Search.Comet("hu")).map(_.take(5)) shouldEqual List(
      Row(EK.Comet("67P"), "Churyumov-Gerasimenko"),
      Row(EK.Comet("106P"), "Schuster"),
      Row(EK.Comet("130P"), "McNaught-Hughes"),
      Row(EK.Comet("178P"), "Hug-Bell"),
      Row(EK.Comet("C/1880 Y1"), "Pechule")
    ).asRight
  }

  test("comet search should handle single result (Format 1) Hubble (C/1937 P1)", RequiresNetwork) {
    runSearch(Search.Comet("hubble")) shouldEqual List(
      Row(EK.Comet("C/1937 P1"), "Hubble")
    ).asRight
  }

  test("comet search should handle single result (Format 2) 1P/Halley pattern", RequiresNetwork) {
    runSearch(Search.Comet("halley")) shouldEqual List(
      Row(EK.Comet("1P"), "Halley")
    ).asRight
  }

  test("asteroid search should handle empty results", RequiresNetwork) {
    runSearch(Search.Asteroid("kjhdwekuq")) shouldEqual Nil.asRight
  }

  test("handle multiple results", RequiresNetwork) {
    runSearch(Search.Asteroid("her")).map(_.take(5)) shouldEqual List(
      Row(EK.AsteroidOld(103), "Hera"),
      Row(EK.AsteroidOld(121), "Hermione"),
      Row(EK.AsteroidOld(135), "Hertha"),
      Row(EK.AsteroidOld(206), "Hersilia"),
      Row(EK.AsteroidOld(214), "Aschera")
    ).asRight
  }

  test("handle single result (Format 1) 90377 Sedna (2003 VB12)", RequiresNetwork) {
    runSearch(Search.Asteroid("sedna")) shouldEqual List(
      Row(EK.AsteroidNew("2003 VB12"), "Sedna")
    ).asRight
  }

  test("handle single result (Format 2) 29 Amphitrite", RequiresNetwork) {
    runSearch(Search.Asteroid("amphitrite")) shouldEqual List(
      Row(EK.AsteroidOld(29), "Amphitrite")
    ).asRight
  }

  test("handle single result (Format 3) (2016 GB222)", RequiresNetwork) {
    runSearch(Search.Asteroid("2016 GB222")) shouldEqual List(
      Row(EK.AsteroidNew("2016 GB222"), "2016 GB222")
    ).asRight
  }

  test("handle single result (Format 4) 418993 (2009 MS9)", RequiresNetwork) {
    runSearch(Search.Asteroid("2009 MS9")) shouldEqual List(
      Row(EK.AsteroidNew("2009 MS9"), "2009 MS9")
    ).asRight
  }

}
