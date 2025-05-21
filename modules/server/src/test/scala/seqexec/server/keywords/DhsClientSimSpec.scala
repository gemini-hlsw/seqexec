// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.IO
import org.typelevel.log4cats.noop.NoOpLogger
import seqexec.model.enums.KeywordName
import seqexec.server.keywords.DhsClient.Permanent

import java.time.LocalDate

class DhsClientSimSpec extends munit.CatsEffectSuite {
  private implicit def logger = NoOpLogger.impl[IO]

  test("produce data labels for today") {
    (DhsClientSim[IO](LocalDate.of(2016, 4, 15))
      .flatMap(_.createImage(DhsClient.ImageParameters(Permanent, Nil)))
      .unsafeRunSync(): String) match {
      case "S20160415S0001" => assert(true)
      case _                => fail("Bad id")
    }
  }
  test("accept keywords") {
    assertEquals(
      (for {
        client <- DhsClientSim.apply[IO]
        id     <- client.createImage(DhsClient.ImageParameters(Permanent, Nil))
        _      <- client.setKeywords(id,
                                     KeywordBag(Int32Keyword(KeywordName.TELESCOP, 10)),
                                     finalFlag = true
                  )
      } yield ()).unsafeRunSync(),
      ()
    )
  }

}
