package edu.gemini.seqexec.web.client.model

import diode.RootModelR
import diode.data._
import edu.gemini.seqexec.model.Model.{Instrument, SequenceView}
import edu.gemini.seqexec.web.common.ArbitrariesWebCommon
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, _}

import scalaz._
import Scalaz._

trait ArbitrariesWebClient extends ArbitrariesWebCommon {
  import edu.gemini.seqexec.model.SharedModelArbitraries._

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
        i   <- arbitrary[Instrument]
        idx <- arbitrary[Option[Int]]
        sv  <- arbitrary[Option[SequenceView]]
      } yield SequenceTab(i, RefTo(new RootModelR(sv.map(k => k.copy(metadata = k.metadata.copy(instrument = i))))), idx)
    }

  implicit val arbSequenceOnDisplay: Arbitrary[SequencesOnDisplay] =
    Arbitrary {
      for {
        s <- Gen.nonEmptyListOf(arbitrary[SequenceTab])
        if s.exists(_.sequence().isDefined)
      } yield {
        val sequences = NonEmptyList(s.head, s.tail: _*)
        SequencesOnDisplay(sequences.toZipper)
      }
    }
}
