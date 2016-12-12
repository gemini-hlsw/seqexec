package edu.gemini.seqexec.web.client.model

import diode.data._
import edu.gemini.seqexec.web.common.ArbitrariesWebCommon
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, _}

import scalaz._
import Scalaz._

trait ArbitrariesWebClient extends ArbitrariesWebCommon {

  implicit def arbPot[A](implicit a: Arbitrary[A]): Arbitrary[Pot[A]] =
    Arbitrary {
      for {
        a  <- arbitrary[A]
        i  <- Gen.oneOf(Empty, Ready(a), Pending(), PendingStale(a), Failed(new RuntimeException()), FailedStale(a, new RuntimeException()))
      } yield i
    }

  implicit val arbSequenceTab: Arbitrary[SequenceTab] =
    Arbitrary {
      for {
        i <- Gen.oneOf(InstrumentNames.instruments.list.toList)
        idx <- arbitrary[Option[Int]]
      } yield SequenceTab(i, SequencesOnDisplay.emptySeqRef, idx)
    }

  implicit val arbSequenceOnDisplay: Arbitrary[SequencesOnDisplay] =
    Arbitrary {
      for {
        s <- Gen.nonEmptyListOf(arbitrary[SequenceTab])
      } yield SequencesOnDisplay(NonEmptyList(s.head, s.tail: _*).toZipper)
    }
}
