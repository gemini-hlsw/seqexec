// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.data.NonEmptyList
import diode.RootModelR
import diode.data._
import gem.arb.ArbObservation
import gem.Observation
import seqexec.model.enum.Instrument
import seqexec.model.{ SequenceState, SequenceView, Step }
import seqexec.model.SharedModelArbitraries._
import seqexec.web.client.model._
import seqexec.web.client.circuit._
import seqexec.model.UserDetails
import seqexec.web.common.Zipper
import seqexec.web.client.components.sequence.steps.OffsetFns.OffsetsDisplay
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, _}
import org.scalajs.dom.WebSocket

trait ArbitrariesWebClient extends ArbObservation {

  implicit val arbInstrumentSequenceTab: Arbitrary[InstrumentSequenceTab] =
    Arbitrary {
      for {
        i   <- arbitrary[Instrument]
        idx <- arbitrary[Option[Int]]
        sv  <- arbitrary[Option[SequenceView]]
        pr  <- arbitrary[Option[SequenceView]]
      } yield InstrumentSequenceTab(i, RefTo(new RootModelR(sv.map(k => k.copy(metadata = k.metadata.copy(instrument = i))))), pr, idx)
    }

  implicit val istCogen: Cogen[InstrumentSequenceTab] =
    Cogen[(Instrument, Option[SequenceView], Option[SequenceView], Option[Int])].contramap {
      x => (x.inst, x.currentSequence(), x.completedSequence, x.stepConfig)
    }

  implicit val arbPreviewSequenceTab: Arbitrary[PreviewSequenceTab] =
    Arbitrary {
      for {
        idx <- arbitrary[Option[Int]]
        sv  <- arbitrary[Option[SequenceView]]
      } yield PreviewSequenceTab(RefTo(new RootModelR(sv)), idx)
    }

  implicit val pstCogen: Cogen[PreviewSequenceTab] =
    Cogen[(Option[SequenceView], Option[Int])].contramap {
      x => (x.currentSequence(), x.stepConfig)
    }

  implicit val arbSequenceTab: Arbitrary[SequenceTab] = Arbitrary {
    Gen.frequency(10 -> arbitrary[InstrumentSequenceTab], 1 -> arbitrary[PreviewSequenceTab])
  }

  implicit val stCogen: Cogen[SequenceTab] =
    Cogen[Either[PreviewSequenceTab, InstrumentSequenceTab]].contramap {
      case a: InstrumentSequenceTab => Right(a)
      case b: PreviewSequenceTab    => Left(b)
    }

  implicit val arbSequenceOnDisplay: Arbitrary[SequencesOnDisplay] =
    Arbitrary {
      for {
        s <- Gen.nonEmptyListOf(arbitrary[SequenceTab])
        if s.exists(_.sequence.isDefined)
      } yield {
        val sequences = NonEmptyList.of(s.headOption.getOrElse(SequenceTab.Empty), s.drop(1): _*)
        SequencesOnDisplay(Zipper.fromNel(sequences))
      }
    }

  implicit val arbOffsetsDisplay: Arbitrary[OffsetsDisplay] =
    Arbitrary {
      for {
        s <- Gen.option(Gen.posNum[Int])
      } yield s.fold(OffsetsDisplay.NoDisplay: OffsetsDisplay)(OffsetsDisplay.DisplayOffsets.apply)
    }

  implicit val odCogen: Cogen[OffsetsDisplay] =
    Cogen[Option[Int]].contramap {
      case OffsetsDisplay.NoDisplay         => None
      case OffsetsDisplay.DisplayOffsets(i) => Some(i)
    }

  implicit val arbWebSocket: Arbitrary[WebSocket] =
    Arbitrary {
      new WebSocket("ws://localhost:9090")
    }

  implicit val wsCogen: Cogen[WebSocket] =
    Cogen[String].contramap(_.url)

  implicit def potArbitrary[A: Arbitrary]: Arbitrary[Pot[A]] =
    Arbitrary(Gen.oneOf(Gen.const(Empty), Gen.const(Unavailable), arbitrary[A].map(Ready.apply), Gen.const(Pending()), arbitrary[A].map(PendingStale(_)), arbitrary[Throwable].map(Failed(_)), arbitrary[(A, Throwable)].map{ case (a, t) => FailedStale(a, t)}))

  implicit def potCogen[A: Cogen]: Cogen[Pot[A]] =
    Cogen[Option[Option[Either[Long, Either[A, Either[(A, Long), Either[Throwable, (A, Throwable)]]]]]]].contramap {
      case Empty              => None
      case Unavailable        => Some(None)
      case Pending(a)         => Some(Some(Left(a)))
      case Ready(a)           => Some(Some(Right(Left(a))))
      case PendingStale(a, l) => Some(Some(Right(Right(Left((a, l))))))
      case Failed(t)          => Some(Some(Right(Right(Right(Left(t))))))
      case FailedStale(a, t)  => Some(Some(Right(Right(Right(Right((a, t)))))))
    }

  implicit val arbWebSocketConnection: Arbitrary[WebSocketConnection] =
    Arbitrary {
      for {
        ws <- arbitrary[Pot[WebSocket]]
        a  <- arbitrary[Int]
        r  <- arbitrary[Boolean]
      } yield WebSocketConnection(ws, a, r)
    }

  implicit val wssCogen: Cogen[WebSocketConnection] =
    Cogen[(Pot[WebSocket], Int, Boolean)].contramap(x => (x.ws, x.nextAttempt, x.autoReconnect))

  implicit val arbClientStatus: Arbitrary[ClientStatus] =
    Arbitrary {
      for {
        u  <- arbitrary[Option[UserDetails]]
        ws <- arbitrary[WebSocketConnection]
        r  <- arbitrary[Boolean]
        s  <- arbitrary[Boolean]
      } yield ClientStatus(u, ws, r, s)
    }

  implicit val cssCogen: Cogen[ClientStatus] =
    Cogen[(Option[UserDetails], WebSocketConnection, Boolean)].contramap(x => (x.u, x.w, x.anySelected))

  implicit val arbStepsTableFocus: Arbitrary[StepsTableFocus] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        i  <- arbitrary[Instrument]
        ss <- arbitrary[SequenceState]
        s  <- arbitrary[List[Step]]
        n  <- arbitrary[Option[Int]]
        e  <- arbitrary[Option[Int]]
        p  <- arbitrary[Boolean]
      } yield StepsTableFocus(id, i, ss, s, n, e, p)
    }

  implicit val sstCogen: Cogen[StepsTableFocus] =
    Cogen[(Observation.Id, Instrument, SequenceState, List[Step], Option[Int], Option[Int])].contramap { x =>
      (x.id, x.instrument, x.state, x.steps, x.stepConfigDisplayed, x.nextStepToRun)
    }

  implicit val arbRunningStep: Arbitrary[RunningStep] =
    Arbitrary {
      for {
        l <- arbitrary[Int]
        i <- arbitrary[Int]
      } yield RunningStep(l, i)
    }

  implicit val runningStepCogen: Cogen[RunningStep] =
    Cogen[(Int, Int)].contramap(x => (x.last, x.total))

  implicit val arbSection: Arbitrary[SectionVisibilityState] =
    Arbitrary(Gen.oneOf(SectionOpen, SectionClosed))

  implicit val sectionCogen: Cogen[SectionVisibilityState] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbStcf: Arbitrary[SequenceTabContentFocus] =
    Arbitrary {
      for {
        i <- arbitrary[Option[Instrument]]
        d <- arbitrary[Option[Observation.Id]]
        s <- arbitrary[Boolean]
        l <- arbitrary[SectionVisibilityState]
      } yield SequenceTabContentFocus(i, d, s, l)
    }

  implicit val stcfCogen: Cogen[SequenceTabContentFocus] =
    Cogen[(Option[Instrument], Option[Observation.Id], Boolean, SectionVisibilityState)]
      .contramap(x => (x.instrument, x.id, x.sequenceSelected, x.logDisplayed))

  implicit val arbAvailableTab: Arbitrary[AvailableTab] =
    Arbitrary {
      for {
        d <- arbitrary[Option[Observation.Id]]
        s <- arbitrary[Option[SequenceState]]
        i <- arbitrary[Option[Instrument]]
        r <- arbitrary[Option[RunningStep]]
        p <- arbitrary[Boolean]
        a <- arbitrary[Boolean]
      } yield AvailableTab(d, s, i, r, p, a)
    }

  implicit val availableTabCogen: Cogen[AvailableTab] =
    Cogen[(Option[Observation.Id], Option[SequenceState], Option[Instrument], Option[RunningStep], Boolean, Boolean)]
      .contramap(x => (x.id, x.status, x.instrument, x.runningStep, x.isPreview, x.active))

  implicit val arbSequenceTabActive: Arbitrary[SequenceTabActive] =
    Arbitrary {
      for {
        d <- arbitrary[SequenceTab]
        a <- arbitrary[Boolean]
      } yield SequenceTabActive(d, a)
    }

  implicit val sequenceTabActiveCogen: Cogen[SequenceTabActive] =
    Cogen[(SequenceTab, Boolean)]
      .contramap(x => (x.tab, x.active))
}
