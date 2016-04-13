package edu.gemini.seqexec.web.client.model

import edu.gemini.seqexec.web.common.{ArbitrariesWebCommon, Sequence}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, _}

trait ArbitrariesWebClient extends ArbitrariesWebCommon {
  implicit val arbSequenceTab: Arbitrary[SequenceTab] =
    Arbitrary {
      for {
        i <- Gen.oneOf("GPI", "F2", "GMOS-S")
        s <- arbitrary[Option[Sequence]]
      } yield SequenceTab(i, s)
    }
}
