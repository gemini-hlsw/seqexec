// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import java.time.ZoneId

class TimeZonesSpec extends AnyFlatSpec with Matchers {
  "TimeZones" should
    "include Santiago" in {
      ZoneId.of("America/Santiago") should not be(null)
    }
    it should "include UTC" in {
      ZoneId.of("UTC") should not be(null)
    }
    it should "include Honolulu" in {
      ZoneId.of("Pacific/Honolulu") should not be(null)
    }
}
