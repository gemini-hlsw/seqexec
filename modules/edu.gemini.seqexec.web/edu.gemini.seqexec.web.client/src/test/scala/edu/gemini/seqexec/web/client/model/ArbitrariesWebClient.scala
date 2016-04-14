package edu.gemini.seqexec.web.client.model

import edu.gemini.seqexec.web.common.{ArbitrariesWebCommon, Instrument, Sequence}
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, _}

import scalaz._
import Scalaz._

trait ArbitrariesWebClient extends ArbitrariesWebCommon {
  implicit val arbSequenceTab: Arbitrary[SequenceTab] =
    Arbitrary {
      for {
        i <- Gen.oneOf(Instrument.instruments)
        s <- arbitrary[Option[Sequence]]
      } yield SequenceTab(i, s.map(_.copy(instrument = i)))
    }

  implicit val arbSequenceOnDisplay: Arbitrary[SequencesOnDisplay] =
    Arbitrary {
      for {
        s <- Gen.nonEmptyListOf(arbitrary[SequenceTab])
      } yield SequencesOnDisplay(NonEmptyList(s.head, s.tail: _*).toZipper)
    }
}
