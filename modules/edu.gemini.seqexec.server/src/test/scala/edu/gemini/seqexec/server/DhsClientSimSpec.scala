// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import java.time.LocalDate

import edu.gemini.seqexec.server.DhsClient.{Int32Keyword, KeywordBag, Permanent}
import org.scalatest.{FlatSpec, Matchers}

import scalaz.\/-

class DhsClientSimSpec extends FlatSpec with Matchers {
  "DhsClientSim" should "produce data labels for today" in {
      DhsClientSim(LocalDate.of(2016, 4, 15)).createImage(DhsClient.ImageParameters(Permanent, Nil)).run.unsafePerformSync should matchPattern {
        case \/-("S20160415S0001") =>
      }
    }
  it should "accept keywords" in {
    val client = DhsClientSim(LocalDate.of(2016, 4, 15))
    client.createImage(DhsClient.ImageParameters(Permanent, Nil)).run.unsafePerformSync.fold(
      _ => fail(),
      id => client.setKeywords(id, KeywordBag(Int32Keyword("Key", 10)), finalFlag = true).run.unsafePerformSync should matchPattern {
        case \/-(()) =>
      }
    )
  }
}
