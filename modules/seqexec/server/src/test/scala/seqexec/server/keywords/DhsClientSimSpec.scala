// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import java.time.LocalDate

import seqexec.server.keywords.DhsClient.{Int32Keyword, KeywordBag, Permanent}
import org.scalatest.{FlatSpec, Matchers}

class DhsClientSimSpec extends FlatSpec with Matchers {
  "DhsClientSim" should "produce data labels for today" in {
      DhsClientSim(LocalDate.of(2016, 4, 15)).createImage(DhsClient.ImageParameters(Permanent, Nil)).value.unsafeRunSync() should matchPattern {
        case Right("S20160415S0001") =>
      }
    }
  it should "accept keywords" in {
    val client = DhsClientSim(LocalDate.of(2016, 4, 15))
    client.createImage(DhsClient.ImageParameters(Permanent, Nil)).value.unsafeRunSync().fold(
      _ => fail(),
      id => client.setKeywords(id, KeywordBag(Int32Keyword("Key", 10)), finalFlag = true).value.unsafeRunSync() should matchPattern {
        case Right(()) =>
      }
    )
  }
}
