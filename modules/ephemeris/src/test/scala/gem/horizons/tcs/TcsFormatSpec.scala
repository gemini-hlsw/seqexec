// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.horizons
package tcs

import cats.tests.CatsSuite


final class TcsFormatSpec extends CatsSuite with EphemerisTestSupport {

  private val Eph = eph(
    "2018-Jan-08 09:43:00.000" -> (("15 02 51.4497 -16 08 04.858",  "26.51632", "-8.34743")),
    "2018-Jan-08 09:44:00.000" -> (("01 30 59.1212  08 54 25.405",   "0.76957",  "0.35575")),
    "2018-Jan-08 09:45:00.000" -> (("22 59 58.5440 -16 25 54.600", "-57.80276", "29.71677"))
  )

  private val Str = List(
    " 2018-Jan-08 09:43 2458126.904861111     15 02 51.4497 -16 08 04.858  26.51632  -8.34743",
    " 2018-Jan-08 09:44 2458126.905555556     01 30 59.1212  08 54 25.405   0.76957   0.35575",
    " 2018-Jan-08 09:45 2458126.906250000     22 59 58.5440 -16 25 54.600 -57.80276  29.71677"
  )

  test("formatting elements") {
    Eph.toList.zip(Str).foreach { case (e, s) =>
      assert(TcsFormat.formatElement(e) == s)
    }
  }
}
