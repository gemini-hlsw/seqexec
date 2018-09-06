// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.data.NonEmptyList
import diode.data._
import gem.arb.ArbObservation
import gem.arb.ArbEnumerated._
import gem.Observation
import gem.enum.Site
import seqexec.model.enum.Instrument
import seqexec.model.{ ClientID, Observer, TargetName, SequencesQueue, SequenceState, SequenceView }
import seqexec.model.{ Notification, Step, UserDetails }
import seqexec.model.events.ServerLogMessage
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.SequenceEventsArbitraries.{slmArb, slmCogen}
import seqexec.web.common.{ FixedLengthBuffer, Zipper }
import seqexec.web.common.ArbitrariesWebCommon._
import seqexec.web.client.model._
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages._
import seqexec.web.client.components.sequence.steps.OffsetFns.OffsetsDisplay
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.QueueTableBody
import org.scalacheck.Arbitrary._
import org.scalacheck.{Arbitrary, _}
import org.scalajs.dom.WebSocket
import web.client.table.{ TableArbitraries, TableState }

trait ArbitrariesWebClient extends ArbObservation with TableArbitraries {

  implicit val arbInstrumentSequenceTab: Arbitrary[InstrumentSequenceTab] =
    Arbitrary {
      for {
        i   <- arbitrary[Instrument]
        idx <- arbitrary[Option[Int]]
        sv  <- arbitrary[Option[SequenceView]]
        pr  <- arbitrary[Option[SequenceView]]
      } yield InstrumentSequenceTab(i, sv.map(k => k.copy(metadata = k.metadata.copy(instrument = i))), pr, idx)
    }

  implicit val istCogen: Cogen[InstrumentSequenceTab] =
    Cogen[(Instrument, Option[SequenceView], Option[SequenceView], Option[Int])].contramap {
      x => (x.inst, x.currentSequence, x.completedSequence, x.stepConfig)
    }

  implicit val arbPreviewSequenceTab: Arbitrary[PreviewSequenceTab] =
    Arbitrary {
      for {
        idx <- arbitrary[Option[Int]]
        sv  <- arbitrary[Option[SequenceView]]
        lo  <- arbitrary[Boolean]
      } yield PreviewSequenceTab(sv, idx, lo)
    }

  implicit val pstCogen: Cogen[PreviewSequenceTab] =
    Cogen[(Option[SequenceView], Option[Int])].contramap {
      x => (x.currentSequence, x.stepConfig)
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

  implicit val sequencesOnDisplayCogen: Cogen[SequencesOnDisplay] =
    Cogen[Zipper[SequenceTab]]
      .contramap(_.sequences)

  implicit val arbOffsetsDisplay: Arbitrary[OffsetsDisplay] =
    Arbitrary {
      for {
        s <- Gen.option(Gen.posNum[Double])
      } yield s.fold(OffsetsDisplay.NoDisplay: OffsetsDisplay)(OffsetsDisplay.DisplayOffsets.apply)
    }

  implicit val odCogen: Cogen[OffsetsDisplay] =
    Cogen[Option[Double]].contramap {
      case OffsetsDisplay.NoDisplay         => None
      case OffsetsDisplay.DisplayOffsets(i) => Some(i)
    }

  implicit val arbWebSocket: Arbitrary[WebSocket] =
    Arbitrary {
      new WebSocket("ws://localhost:9090")
    }

  implicit val wsCogen: Cogen[WebSocket] =
    Cogen[String].contramap(_.url)

  implicit def pendingStaleGen[A: Arbitrary]: Arbitrary[PendingStale[A]] =
    Arbitrary {
      for {
        a <- arbitrary[A]
        t <- Gen.posNum[Long]
      } yield PendingStale(a, t)
    }

  implicit def potArbitrary[A: Arbitrary]: Arbitrary[Pot[A]] =
    Arbitrary(Gen.oneOf(Gen.const(Empty), Gen.const(Unavailable), arbitrary[A].map(Ready.apply), Gen.const(Pending()), arbitrary[PendingStale[A]], arbitrary[Throwable].map(Failed(_)), arbitrary[(A, Throwable)].map{ case (a, t) => FailedStale(a, t)}))

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
        s  <- arbitrary[Boolean]
      } yield ClientStatus(u, ws, s)
    }

  implicit val cssCogen: Cogen[ClientStatus] =
    Cogen[(Option[UserDetails], WebSocketConnection, Boolean)].contramap(x => (x.u, x.w, x.syncInProgress))

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
        g <- arbitrary[Boolean]
        i <- arbitrary[Option[Instrument]]
        d <- arbitrary[Option[Observation.Id]]
        s <- arbitrary[Boolean]
        l <- arbitrary[SectionVisibilityState]
      } yield SequenceTabContentFocus(g, i, d, s, l)
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
        l <- arbitrary[Boolean]
      } yield AvailableTab(d, s, i, r, p, a, l)
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

  implicit val arbSequencesFocus: Arbitrary[SequencesFocus] =
    Arbitrary {
      for {
        s <- arbitrary[SequencesQueue[SequenceView]]
        d <- arbitrary[SequencesOnDisplay]
      } yield SequencesFocus(s, d)
    }

  implicit val sfCogen: Cogen[SequencesFocus] =
    Cogen[(SequencesQueue[SequenceView], SequencesOnDisplay)].contramap { x =>
      (x.sequences, x.sod)
    }

  implicit val arbSequenceInfoFocus: Arbitrary[SequenceInfoFocus] =
    Arbitrary {
      for {
        l <- arbitrary[Boolean]
        n <- arbitrary[Option[String]]
        s <- arbitrary[Option[SequenceState]]
        t <- arbitrary[Option[TargetName]]
      } yield SequenceInfoFocus(l, n, s, t)
    }

  implicit val sifCogen: Cogen[SequenceInfoFocus] =
    Cogen[(Boolean, Option[String], Option[SequenceState], Option[TargetName])].contramap { x =>
      (x.isLogged, x.obsName, x.status, x.targetName)
    }

  implicit val arbPreviewPage: Arbitrary[PreviewPage] =
    Arbitrary {
      for {
        i  <- arbitrary[Instrument]
        oi <- arbitrary[Observation.Id]
        sd <- arbitrary[Int]
      } yield PreviewPage(i, oi, sd)
    }

  implicit val previewPageCogen: Cogen[PreviewPage] =
    Cogen[(Instrument, Observation.Id, Int)]
      .contramap(x => (x.instrument, x.obsId, x.step))

  implicit val arbSequencePage: Arbitrary[SequencePage] =
    Arbitrary {
      for {
        i  <- arbitrary[Instrument]
        oi <- arbitrary[Observation.Id]
        sd <- arbitrary[Int]
      } yield SequencePage(i, oi, sd)
    }

  implicit val sequencePageCogen: Cogen[SequencePage] =
    Cogen[(Instrument, Observation.Id, Int)]
      .contramap(x => (x.instrument, x.obsId, x.step))

  implicit val arbPreviewConfigPage: Arbitrary[PreviewConfigPage] =
    Arbitrary {
      for {
        i  <- arbitrary[Instrument]
        oi <- arbitrary[Observation.Id]
        st <- Gen.posNum[Int]
      } yield PreviewConfigPage(i, oi, st)
    }

  implicit val previewConfigPageCogen: Cogen[PreviewConfigPage] =
    Cogen[(Instrument, Observation.Id, Int)]
      .contramap(x => (x.instrument, x.obsId, x.step))

  implicit val arbSequenceConfigPage: Arbitrary[SequenceConfigPage] =
    Arbitrary {
      for {
        i  <- arbitrary[Instrument]
        oi <- arbitrary[Observation.Id]
        st <- Gen.posNum[Int]
      } yield SequenceConfigPage(i, oi, st)
    }

  implicit val sequenceConfigPageCogen: Cogen[SequenceConfigPage] =
    Cogen[(Instrument, Observation.Id, Int)]
      .contramap(x => (x.instrument, x.obsId, x.step))

  implicit val arbSeqexecPages: Arbitrary[SeqexecPages] =
    Arbitrary {
      for {
        r  <- Gen.const(Root)
        sc <- Gen.const(SoundTest)
        ep <- Gen.const(EmptyPreviewPage)
        pp <- arbitrary[PreviewPage]
        sp <- arbitrary[SequencePage]
        pc <- arbitrary[PreviewConfigPage]
        sc <- arbitrary[SequenceConfigPage]
        p  <- Gen.oneOf(r, sc, ep, pp, sp, pc, sc)
      } yield p
    }

  implicit val seqexecPageCogen: Cogen[SeqexecPages] =
    Cogen[Option[Option[Option[Either[(Instrument, Observation.Id, Int), Either[(Instrument, Observation.Id, Int), Either[(Instrument, Observation.Id, Int), (Instrument, Observation.Id, Int)]]]]]]].contramap {
      case Root                        => None
      case SoundTest                   => Some(None)
      case EmptyPreviewPage            => Some(Some(None))
      case PreviewPage(i, o, s)        => Some(Some(Some(Left((i, o, s)))))
      case SequencePage(i, o, s)       => Some(Some(Some(Right(Left((i, o, s))))))
      case SequenceConfigPage(i, o, s) => Some(Some(Some(Right(Right(Left((i, o, s)))))))
      case PreviewConfigPage(i, o, s)  => Some(Some(Some(Right(Right(Right((i, o, s)))))))
    }

  implicit val arbUserNotificationState: Arbitrary[UserNotificationState] =
    Arbitrary {
      for {
        v <- arbitrary[SectionVisibilityState]
        n <- arbitrary[Option[Notification]]
      } yield UserNotificationState(v, n)
    }

  implicit val userNotificationCogen: Cogen[UserNotificationState] =
    Cogen[(SectionVisibilityState, Option[Notification])].contramap(x => (x.visibility, x.notification))

  implicit val arbGlobalLog: Arbitrary[GlobalLog] =
    Arbitrary {
      for {
        b  <- arbitrary[FixedLengthBuffer[ServerLogMessage]]
        v  <- arbitrary[SectionVisibilityState]
      } yield GlobalLog(b, v)
    }

  implicit val globalLogCogen: Cogen[GlobalLog] =
    Cogen[(FixedLengthBuffer[ServerLogMessage], SectionVisibilityState)].contramap(x => (x.log, x.display))

  implicit val arbStepConfigTableTableColumn: Arbitrary[StepConfigTable.TableColumn] =
    Arbitrary(Gen.oneOf(StepConfigTable.NameColumn, StepConfigTable.ValueColumn))

  implicit val stepConfigTableColumnCogen: Cogen[StepConfigTable.TableColumn] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbQueueTableBodyTableColumn: Arbitrary[QueueTableBody.TableColumn] =
    Arbitrary(Gen.oneOf(QueueTableBody.all.map(_.column).toList))

  implicit val queueTableBodyTableColumnCogen: Cogen[QueueTableBody.TableColumn] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbSeqexecUIModel: Arbitrary[SeqexecUIModel] =
    Arbitrary {
      for {
        navLocation        <- arbitrary[Pages.SeqexecPages]
        user               <- arbitrary[Option[UserDetails]]
        sequences          <- arbitrary[SequencesQueue[SequenceView]]
        loginBox           <- arbitrary[SectionVisibilityState]
        globalLog          <- arbitrary[GlobalLog]
        sequencesOnDisplay <- arbitrary[SequencesOnDisplay]
        syncInProgress     <- arbitrary[Boolean]
        configTableState   <- arbitrary[TableState[StepConfigTable.TableColumn]]
        queueTableState    <- arbitrary[TableState[QueueTableBody.TableColumn]]
        defaultObserver    <- arbitrary[Observer]
        notification       <- arbitrary[UserNotificationState]
        firstLoad          <- arbitrary[Boolean]
      } yield SeqexecUIModel(navLocation, user, sequences, loginBox, globalLog, sequencesOnDisplay, syncInProgress, configTableState, queueTableState, defaultObserver, notification, firstLoad)
    }

  implicit val seqUIModelCogen: Cogen[SeqexecUIModel] =
    Cogen[(Pages.SeqexecPages, Option[UserDetails], SequencesQueue[SequenceView], SectionVisibilityState, GlobalLog, SequencesOnDisplay, Boolean, TableState[StepConfigTable.TableColumn], TableState[QueueTableBody.TableColumn], Observer, UserNotificationState, Boolean)]
      .contramap(x => (x.navLocation, x.user, x.sequences, x.loginBox, x.globalLog, x.sequencesOnDisplay, x.syncInProgress, x.configTableState, x.queueTableState, x.defaultObserver, x.notification, x.firstLoad))

  implicit val arbSODLocationFocus: Arbitrary[SODLocationFocus] =
    Arbitrary {
      for {
        navLocation        <- arbitrary[Pages.SeqexecPages]
        sequencesOnDisplay <- arbitrary[SequencesOnDisplay]
        clientId           <- arbitrary[Option[ClientID]]
      } yield SODLocationFocus(navLocation, sequencesOnDisplay, clientId)
    }

  implicit val sodLocationFocusogen: Cogen[SODLocationFocus] =
    Cogen[(Pages.SeqexecPages, SequencesOnDisplay, Option[ClientID])].contramap(x => (x.location, x.sod, x.clientId))

  implicit val arbInitialSyncFocus: Arbitrary[InitialSyncFocus] =
    Arbitrary {
      for {
        navLocation        <- arbitrary[Pages.SeqexecPages]
        sequencesOnDisplay <- arbitrary[SequencesOnDisplay]
        firstLoad          <- arbitrary[Boolean]
      } yield InitialSyncFocus(navLocation, sequencesOnDisplay, firstLoad)
    }

  implicit val initialSyncFocusCogen: Cogen[InitialSyncFocus] =
    Cogen[(Pages.SeqexecPages, SequencesOnDisplay, Boolean)].contramap(x => (x.location, x.sod, x.firstLoad))

  implicit val arbSeqexecAppRootModel: Arbitrary[SeqexecAppRootModel] =
    Arbitrary {
      for {
        ws       <- arbitrary[WebSocketConnection]
        site     <- arbitrary[Option[Site]]
        clientId <- arbitrary[Option[ClientID]]
        uiModel  <- arbitrary[SeqexecUIModel]
      } yield SeqexecAppRootModel(ws, site, clientId, uiModel)
    }

}
