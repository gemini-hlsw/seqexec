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
}
