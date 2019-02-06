// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import gem.enum.KeywordName
import cats.effect.IO
import java.time.LocalDate
import org.scalatest.{FlatSpec, Matchers}
import seqexec.server.keywords.DhsClient.Permanent

class DhsClientSimSpec extends FlatSpec with Matchers {
  "DhsClientSim" should "produce data labels for today" in {
      DhsClientSim[IO](LocalDate.of(2016, 4, 15)).createImage(DhsClient.ImageParameters(Permanent, Nil)).unsafeRunSync() should matchPattern {
        case "S20160415S0001" =>
      }
    }
  it should "accept keywords" in {
    val client = DhsClientSim[IO](LocalDate.of(2016, 4, 15))
    client.createImage(DhsClient.ImageParameters(Permanent, Nil)).flatMap { id =>
      client.setKeywords(id, KeywordBag(Int32Keyword(KeywordName.TELESCOP, 10)), finalFlag = true)
    }.unsafeRunSync() shouldEqual (())
  }
}
