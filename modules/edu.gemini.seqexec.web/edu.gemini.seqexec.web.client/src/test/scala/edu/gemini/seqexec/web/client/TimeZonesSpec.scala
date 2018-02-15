// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client

import org.scalatest.{FlatSpec, Matchers}
import java.time.ZoneId

class TimeZonesSpec extends FlatSpec with Matchers {
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
