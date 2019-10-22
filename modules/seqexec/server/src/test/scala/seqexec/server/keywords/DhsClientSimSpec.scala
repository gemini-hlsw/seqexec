// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.IO
import gem.enum.KeywordName
import io.chrisdavenport.log4cats.noop.NoOpLogger
import java.time.LocalDate
import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import seqexec.server.keywords.DhsClient.Permanent

class DhsClientSimSpec extends AnyFlatSpec with Matchers {
  private implicit def logger = NoOpLogger.impl[IO]

  "DhsClientSim" should "produce data labels for today" in {
      DhsClientSim[IO](LocalDate.of(2016, 4, 15)).flatMap(_.createImage(DhsClient.ImageParameters(Permanent, Nil))).unsafeRunSync() should matchPattern {
        case "S20160415S0001" =>
      }
    }
  it should "accept keywords" in {
    (for {
      client <- DhsClientSim.apply[IO]
      id     <- client.createImage(DhsClient.ImageParameters(Permanent, Nil))
      _      <- client.setKeywords(id, KeywordBag(Int32Keyword(KeywordName.TELESCOP, 10)), finalFlag = true)
    } yield ()).unsafeRunSync() shouldEqual (())
  }

}
