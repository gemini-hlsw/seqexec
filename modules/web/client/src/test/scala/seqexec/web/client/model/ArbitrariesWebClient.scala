// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.data.NonEmptyList
import cats.implicits._
import diode.data._
import seqexec.model.arb.ArbObservationId
import lucuma.core.util.arb.ArbEnumerated._
import seqexec.model.Observation
import lucuma.core.enums.Site
import lucuma.core.data.Zipper
import lucuma.core.data.arb.ArbZipper._
import scala.collection.immutable.SortedMap
import seqexec.model.enum.Instrument
import seqexec.model.enum.Resource
import seqexec.model.enum.BatchExecState
import seqexec.model._
import seqexec.model.events.ServerLogMessage
import seqexec.model.arb.all._
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.SequenceEventsArbitraries.slmArb
import seqexec.model.SequenceEventsArbitraries.slmCogen
import seqexec.model.SequenceEventsArbitraries.qmArb
import seqexec.common.FixedLengthBuffer
import seqexec.common.ArbitrariesCommon._
import seqexec.web.client.model._
import seqexec.web.client.model.SectionVisibilityState._
import seqexec.web.client.circuit._
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.Formatting.OffsetsDisplay
import seqexec.web.client.components.sequence.steps.StepConfigTable
import seqexec.web.client.components.sequence.steps.StepsTable
import seqexec.web.client.components.queue.CalQueueTable
import seqexec.web.client.components.SessionQueueTable
import seqexec.web.client.arb.ArbTabOperations
import shapeless.tag
import org.scalacheck.Arbitrary._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen._
import org.scalacheck._
import org.scalajs.dom.WebSocket
import web.client.table.TableArbitraries
import web.client.table.TableState

trait ArbitrariesWebClient extends ArbObservationId with TableArbitraries with ArbTabOperations {

  implicit val arbQueueOperations: Arbitrary[QueueOperations] =
    Arbitrary {
      for {
        r <- arbitrary[AddDayCalOperation]
        c <- arbitrary[ClearAllCalOperation]
        u <- arbitrary[RunCalOperation]
        s <- arbitrary[StopCalOperation]
      } yield QueueOperations(r, c, u, s)
    }

  implicit val qoCogen: Cogen[QueueOperations] =
    Cogen[AddDayCalOperation].contramap(x => x.addDayCalRequested)

  implicit val arbCalibrationQueueTab: Arbitrary[CalibrationQueueTab] =
    Arbitrary {
      for {
        st <- arbitrary[BatchExecState]
        o  <- arbitrary[Option[Observer]]
      } yield CalibrationQueueTab(st, o)
    }

  implicit val cqtCogen: Cogen[CalibrationQueueTab] =
    Cogen[(BatchExecState, Option[Observer])]
      .contramap { x =>
        (x.state, x.observer)
      }

  implicit val arbQueueSeqOperations: Arbitrary[QueueSeqOperations] =
    Arbitrary {
      for {
        r <- arbitrary[RemoveSeqQueue]
        m <- arbitrary[MoveSeqQueue]
      } yield QueueSeqOperations(r, m)
    }

  implicit val sopCogen: Cogen[QueueSeqOperations] =
    Cogen[(RemoveSeqQueue, MoveSeqQueue)]
      .contramap(x => (x.removeSeqQueue, x.moveSeqQueue))

  implicit val arbInstrumentSequenceTab: Arbitrary[InstrumentSequenceTab] =
    Arbitrary {
      for {
        i   <- arbitrary[Instrument]
        idx <- arbitrary[Option[Int]]
        sv  <- arbitrary[Either[SequenceView, SequenceView]]
        to  <- arbitrary[TabOperations]
        so  <- arbitrary[SystemOverrides]
        se  <- arbitrary[Option[StepId]]
        ov  <- arbitrary[SectionVisibilityState]
      } yield InstrumentSequenceTab(i,
                                    sv.bimap(tag[InstrumentSequenceTab.CompletedSV][SequenceView],
                                             tag[InstrumentSequenceTab.LoadedSV][SequenceView]
                                    ),
                                    idx,
                                    se,
                                    to,
                                    so,
                                    ov
      )
    }

  implicit val istCogen: Cogen[InstrumentSequenceTab] =
    Cogen[
      (
        Instrument,
        SequenceView,
        Option[StepId],
        Option[StepId],
        TabOperations,
        SystemOverrides,
        SectionVisibilityState
      )
    ].contramap { x =>
      (x.inst,
       x.sequence,
       x.stepConfig,
       x.selectedStep,
       x.tabOperations,
       x.systemOverrides,
       x.subsysControls
      )
    }

  implicit val arbPreviewSequenceTab: Arbitrary[PreviewSequenceTab] =
    Arbitrary {
      for {
        idx <- arbitrary[Option[Int]]
        sv  <- arbitrary[SequenceView]
        lo  <- arbitrary[Boolean]
        to  <- arbitrary[TabOperations]
      } yield PreviewSequenceTab(sv, idx, lo, to)
    }

  implicit val pstCogen: Cogen[PreviewSequenceTab] =
    Cogen[(SequenceView, Option[Int], TabOperations)].contramap { x =>
      (x.currentSequence, x.stepConfig, x.tabOperations)
    }

  implicit val arbSeqexecTab: Arbitrary[SeqexecTab] = Arbitrary {
    Gen.frequency(10 -> arbitrary[InstrumentSequenceTab],
                  1  -> arbitrary[PreviewSequenceTab],
                  1  -> arbitrary[CalibrationQueueTab]
    )
  }

  implicit val sxCogen: Cogen[SeqexecTab] =
    Cogen[Either[CalibrationQueueTab, Either[PreviewSequenceTab, InstrumentSequenceTab]]]
      .contramap {
        case a: CalibrationQueueTab   => Left(a)
        case a: PreviewSequenceTab    => Right(Left(a))
        case a: InstrumentSequenceTab => Right(Right(a))
      }

  implicit val arbSequenceTab: Arbitrary[SequenceTab] = Arbitrary {
    Gen.frequency(10 -> arbitrary[InstrumentSequenceTab], 1 -> arbitrary[PreviewSequenceTab])
  }

  implicit val stCogen: Cogen[SequenceTab] =
    Cogen[Either[PreviewSequenceTab, InstrumentSequenceTab]]
      .contramap {
        case a: PreviewSequenceTab    => Left(a)
        case a: InstrumentSequenceTab => Right(a)
      }

  implicit val arbSequenceOnDisplay: Arbitrary[SequencesOnDisplay] =
    Arbitrary {
      for {
        c <- arbitrary[CalibrationQueueTab]
        l <- Gen.chooseNum(0, 4)
        s <- Gen.listOfN(l, arbitrary[SeqexecTab])
      } yield {
        val sequences = NonEmptyList.of(c, s: _*)
        SequencesOnDisplay(Zipper.fromNel(sequences))
      }
    }

  implicit val sequencesOnDisplayCogen: Cogen[SequencesOnDisplay] =
    Cogen[Zipper[SeqexecTab]]
      .contramap(_.tabs)

  private val posNumTuple3Gen: Gen[(Double, Double, Double)] =
    for {
      a1 <- Gen.posNum[Double]
      a2 <- Gen.posNum[Double]
      a3 <- Gen.posNum[Double]
    } yield (a1, a2, a3)

  implicit val arbOffsetsDisplay: Arbitrary[OffsetsDisplay] =
    Arbitrary {
      for {
        s <- Gen.option(posNumTuple3Gen)
      } yield s.fold(OffsetsDisplay.NoDisplay: OffsetsDisplay)(
        (OffsetsDisplay.DisplayOffsets.apply _).tupled
      )
    }

  implicit val odCogen: Cogen[OffsetsDisplay] =
    Cogen[Option[(Double, Double, Double)]].contramap {
      case OffsetsDisplay.NoDisplay                  => None
      case OffsetsDisplay.DisplayOffsets(ow, aw, nw) => Some((ow, aw, nw))
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
    Arbitrary(
      Gen.oneOf(
        Gen.const(Empty),
        Gen.const(Unavailable),
        arbitrary[A].map(Ready.apply),
        Gen.const(Pending()),
        arbitrary[PendingStale[A]],
        arbitrary[Throwable].map(Failed(_)),
        arbitrary[(A, Throwable)].map { case (a, t) => FailedStale(a, t) }
      )
    )

  implicit def potCogen[A: Cogen]: Cogen[Pot[A]] =
    Cogen[Option[
      Option[Either[Long, Either[A, Either[(A, Long), Either[Throwable, (A, Throwable)]]]]]
    ]]
      .contramap {
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
      } yield WebSocketConnection(ws, a, r, None)
    }

  implicit val wssCogen: Cogen[WebSocketConnection] =
    Cogen[(Pot[WebSocket], Int, Boolean)].contramap(x => (x.ws, x.nextAttempt, x.autoReconnect))

  implicit val arbClientStatus: Arbitrary[ClientStatus] =
    Arbitrary {
      for {
        u <- arbitrary[Option[UserDetails]]
        c <- arbitrary[Option[ClientId]]
        d <- arbitrary[Map[String, String]]
        w <- arbitrary[WebSocketConnection]
      } yield ClientStatus(u, c, d, w)
    }

  implicit val cssCogen: Cogen[ClientStatus] =
    Cogen[(Option[UserDetails], Option[ClientId], Map[String, String], WebSocketConnection)]
      .contramap(x => (x.user, x.clientId, x.displayNames, x.w))

  implicit val arbTableType: Arbitrary[StepsTableTypeSelection] =
    Arbitrary(
      Gen.oneOf(Gen.const(StepsTableTypeSelection.StepsTableSelected),
                Gen
                  .posNum[Int]
                  .map(StepsTableTypeSelection.StepConfigTableSelected.apply)
      )
    )

  implicit val tableTypeCogen: Cogen[StepsTableTypeSelection] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbStcf: Arbitrary[SequenceTabContentFocus] =
    Arbitrary {
      for {
        g <- arbitrary[Boolean]
        i <- arbitrary[Instrument]
        d <- arbitrary[Observation.Id]
        s <- arbitrary[TabSelected]
        t <- arbitrary[StepsTableTypeSelection]
        l <- arbitrary[SectionVisibilityState]
        o <- arbitrary[Boolean]
        a <- arbitrary[Int]
      } yield SequenceTabContentFocus(g, i, d, s, t, l, o, a)
    }

  implicit val stcfCogen: Cogen[SequenceTabContentFocus] =
    Cogen[
      (
        Boolean,
        Instrument,
        Observation.Id,
        TabSelected,
        StepsTableTypeSelection,
        SectionVisibilityState,
        Boolean,
        Int
      )
    ]
      .contramap(x =>
        (x.canOperate,
         x.instrument,
         x.id,
         x.active,
         x.tableType,
         x.logDisplayed,
         x.isPreview,
         x.totalSteps
        )
      )

  implicit val arbQtcf: Arbitrary[CalQueueTabContentFocus] =
    Arbitrary {
      for {
        g <- arbitrary[Boolean]
        a <- arbitrary[TabSelected]
        s <- arbitrary[SectionVisibilityState]
      } yield CalQueueTabContentFocus(g, a, s)
    }

  implicit val qtcfCogen: Cogen[CalQueueTabContentFocus] =
    Cogen[(Boolean, TabSelected, SectionVisibilityState)]
      .contramap(x => (x.canOperate, x.active, x.logDisplayed))

  implicit val arbtcf: Arbitrary[TabContentFocus] = Arbitrary {
    Gen.frequency(10 -> arbitrary[SequenceTabContentFocus], 4 -> arbitrary[CalQueueTabContentFocus])
  }

  implicit val tcfCogen: Cogen[TabContentFocus] =
    Cogen[Either[CalQueueTabContentFocus, SequenceTabContentFocus]]
      .contramap {
        case t: SequenceTabContentFocus => t.asRight
        case t: CalQueueTabContentFocus => t.asLeft
      }

  implicit val arbAvailableTab: Arbitrary[AvailableTab] =
    Arbitrary {
      for {
        d <- arbitrary[Observation.Id]
        s <- arbitrary[SequenceState]
        i <- arbitrary[Instrument]
        n <- arbitrary[Option[Int]]
        r <- arbitrary[Option[RunningStep]]
        p <- arbitrary[Boolean]
        a <- arbitrary[TabSelected]
        l <- arbitrary[Boolean]
        o <- arbitrary[SortedMap[Resource, ResourceRunOperation]]
        u <- arbitrary[SystemOverrides]
        m <- arbitrary[SectionVisibilityState]
      } yield AvailableTab(d, s, i, r, n, p, a, l, u, m, o)
    }

  implicit val availableTabCogen: Cogen[AvailableTab] =
    Cogen[
      (
        Observation.Id,
        SequenceState,
        Instrument,
        Option[Int],
        Option[RunningStep],
        Boolean,
        TabSelected,
        SystemOverrides,
        SectionVisibilityState
      )
    ]
      .contramap(x =>
        (x.id,
         x.status,
         x.instrument,
         x.nextStepToRun,
         x.runningStep,
         x.isPreview,
         x.active,
         x.systemOverrides,
         x.overrideControls
        )
      )

  implicit val arbSeqexecTabActive: Arbitrary[SeqexecTabActive] =
    Arbitrary {
      for {
        d <- arbitrary[SequenceTab]
        a <- arbitrary[TabSelected]
      } yield SeqexecTabActive(d, a)
    }

  implicit val sequenceTabActiveCogen: Cogen[SeqexecTabActive] =
    Cogen[(SeqexecTab, TabSelected)]
      .contramap(x => (x.tab, x.active))

  implicit val arbStepIdDisplayed: Arbitrary[StepIdDisplayed] =
    Arbitrary(arbitrary[Int].map(StepIdDisplayed.apply))

  implicit val stepDisplayedCogen: Cogen[StepIdDisplayed] =
    Cogen[Int].contramap(_.step)

  implicit val arbStepsTableFocus: Arbitrary[StepsTableFocus] =
    Arbitrary {
      for {
        id <- arbitrary[Observation.Id]
        i  <- arbitrary[Instrument]
        ss <- arbitrary[SequenceState]
        s  <- arbitrary[List[Step]]
        n  <- arbitrary[Option[Int]]
        e  <- arbitrary[Option[Int]]
        se <- arbitrary[Option[StepId]]
        rs <- arbitrary[Option[RunningStep]]
        p  <- arbitrary[Boolean]
        ts <- arbitrary[TableState[StepsTable.TableColumn]]
        to <- arbitrary[TabOperations]
      } yield StepsTableFocus(id, i, ss, s, n, e, se, rs, p, ts, to)
    }

  implicit val sstCogen: Cogen[StepsTableFocus] =
    Cogen[
      (
        Observation.Id,
        Instrument,
        SequenceState,
        List[Step],
        Option[Int],
        Option[StepId],
        Option[StepId],
        TableState[StepsTable.TableColumn]
      )
    ].contramap { x =>
      (x.id,
       x.instrument,
       x.state,
       x.steps,
       x.stepConfigDisplayed,
       x.nextStepToRun,
       x.selectedStep,
       x.tableState
      )
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
        n <- arbitrary[String]
        s <- arbitrary[SequenceState]
        t <- arbitrary[Option[TargetName]]
      } yield SequenceInfoFocus(l, n, s, t)
    }

  implicit val sifCogen: Cogen[SequenceInfoFocus] =
    Cogen[(Boolean, String, SequenceState, Option[TargetName])]
      .contramap { x =>
        (x.canOperate, x.obsName, x.status, x.targetName)
      }

  implicit val arbPreviewPage: Arbitrary[PreviewPage] =
    Arbitrary {
      for {
        i  <- arbitrary[Instrument]
        oi <- arbitrary[Observation.Id]
        sd <- arbitrary[StepIdDisplayed]
      } yield PreviewPage(i, oi, sd)
    }

  implicit val previewPageCogen: Cogen[PreviewPage] =
    Cogen[(Instrument, Observation.Id, StepIdDisplayed)]
      .contramap(x => (x.instrument, x.obsId, x.step))

  implicit val arbSequencePage: Arbitrary[SequencePage] =
    Arbitrary {
      for {
        i  <- arbitrary[Instrument]
        oi <- arbitrary[Observation.Id]
        sd <- arbitrary[StepIdDisplayed]
      } yield SequencePage(i, oi, sd)
    }

  implicit val sequencePageCogen: Cogen[SequencePage] =
    Cogen[(Instrument, Observation.Id, StepIdDisplayed)]
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
        st <- Gen.const(SoundTest)
        ep <- Gen.const(CalibrationQueuePage)
        pp <- arbitrary[PreviewPage]
        sp <- arbitrary[SequencePage]
        pc <- arbitrary[PreviewConfigPage]
        sc <- arbitrary[SequenceConfigPage]
        p  <- Gen.oneOf(r, st, ep, pp, sp, pc, sc)
      } yield p
    }

  implicit val seqexecPageCogen: Cogen[SeqexecPages] =
    Cogen[Option[Option[Option[Either[(Instrument, Observation.Id, StepIdDisplayed), Either[
      (Instrument, Observation.Id, StepIdDisplayed),
      Either[(Instrument, Observation.Id, Int), (Instrument, Observation.Id, Int)]
    ]]]]]]
      .contramap {
        case Root                        => None
        case CalibrationQueuePage        => Some(None)
        case SoundTest                   => Some(Some(None))
        case PreviewPage(i, o, s)        => Some(Some(Some(Left((i, o, s)))))
        case SequencePage(i, o, s)       =>
          Some(Some(Some(Right(Left((i, o, s))))))
        case SequenceConfigPage(i, o, s) =>
          Some(Some(Some(Right(Right(Left((i, o, s)))))))
        case PreviewConfigPage(i, o, s)  =>
          Some(Some(Some(Right(Right(Right((i, o, s)))))))
      }

  implicit val arbUserNotificationState: Arbitrary[UserNotificationState] =
    Arbitrary {
      for {
        v <- arbitrary[SectionVisibilityState]
        n <- arbitrary[Option[Notification]]
      } yield UserNotificationState(v, n)
    }

  implicit val userNotificationCogen: Cogen[UserNotificationState] =
    Cogen[(SectionVisibilityState, Option[Notification])].contramap(x =>
      (x.visibility, x.notification)
    )

  implicit val arbUserPromptState: Arbitrary[UserPromptState] =
    Arbitrary {
      for {
        n <- arbitrary[Option[UserPrompt]]
      } yield UserPromptState(n)
    }

  implicit val userPromptStateCogen: Cogen[UserPromptState] =
    Cogen[Option[UserPrompt]].contramap(_.notification)

  implicit val arbGlobalLog: Arbitrary[GlobalLog] =
    Arbitrary {
      for {
        b <- arbitrary[FixedLengthBuffer[ServerLogMessage]]
        v <- arbitrary[SectionVisibilityState]
      } yield GlobalLog(b, v)
    }

  implicit val globalLogCogen: Cogen[GlobalLog] =
    Cogen[(FixedLengthBuffer[ServerLogMessage], SectionVisibilityState)]
      .contramap(x => (x.log, x.display))

  implicit val arbStepConfigTableTableColumn: Arbitrary[StepConfigTable.TableColumn] =
    Arbitrary(Gen.oneOf(StepConfigTable.NameColumn, StepConfigTable.ValueColumn))

  implicit val stepConfigTableColumnCogen: Cogen[StepConfigTable.TableColumn] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbQueueTableBodyTableColumn: Arbitrary[SessionQueueTable.TableColumn] =
    Arbitrary(Gen.oneOf(SessionQueueTable.all.map(_.column).toList))

  implicit val queueTableBodyTableColumnCogen: Cogen[SessionQueueTable.TableColumn] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbStepsTableTableColumn: Arbitrary[StepsTable.TableColumn] =
    Arbitrary(Gen.oneOf(StepsTable.all.map(_.column).toList))

  implicit val stepsTableColumnCogen: Cogen[StepsTable.TableColumn] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbCalQueueTableTableColumn: Arbitrary[CalQueueTable.TableColumn] =
    Arbitrary(Gen.oneOf(CalQueueTable.ObsIdColumn, CalQueueTable.InstrumentColumn))

  implicit val calTableColumnCogen: Cogen[CalQueueTable.TableColumn] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbCalQueuesState: Arbitrary[CalQueueState] =
    Arbitrary {
      for {
        ops <- arbitrary[QueueOperations]
        ts  <- arbitrary[TableState[CalQueueTable.TableColumn]]
        sop <- arbitrary[SortedMap[Observation.Id, QueueSeqOperations]]
        lOp <- arbitrary[Option[QueueManipulationOp]]
      } yield CalQueueState(ops, ts, sop, lOp)
    }

  implicit val calQueuesStateCogen: Cogen[CalQueueState] =
    Cogen[(QueueOperations, TableState[CalQueueTable.TableColumn])]
      .contramap(x => (x.ops, x.tableState))

  implicit val arbCalQueues: Arbitrary[CalibrationQueues] =
    Arbitrary {
      for {
        ops <- arbitrary[SortedMap[QueueId, CalQueueState]]
      } yield CalibrationQueues(ops)
    }

  implicit val calQueuesCogen: Cogen[CalibrationQueues] =
    Cogen[List[(QueueId, CalQueueState)]].contramap(_.queues.toList)

  implicit val arbAllObservationsProgressState: Arbitrary[AllObservationsProgressState] =
    Arbitrary {
      for {
        ops <- arbitrary[SortedMap[(Observation.Id, StepId), Progress]]
      } yield AllObservationsProgressState(ops)
    }

  implicit val obsProgressCogen: Cogen[AllObservationsProgressState] =
    Cogen[List[((Observation.Id, StepId), Progress)]]
      .contramap(_.obsProgress.toList)

  implicit val arbObsClass: Arbitrary[ObsClass] =
    Arbitrary(Gen.oneOf(ObsClass.All, ObsClass.Daytime, ObsClass.Nighttime))

  implicit val obsClassCogen: Cogen[ObsClass] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbSessionQueueFilter: Arbitrary[SessionQueueFilter] =
    Arbitrary {
      for {
        ce <- arbitrary[ObsClass]
      } yield SessionQueueFilter(ce)
    }

  implicit val obsQueueFilter: Cogen[SessionQueueFilter] =
    Cogen[ObsClass]
      .contramap(_.obsClass)

  implicit val arbSeqexecUIModel: Arbitrary[SeqexecUIModel] =
    Arbitrary {
      for {
        navLocation        <- arbitrary[Pages.SeqexecPages]
        user               <- arbitrary[Option[UserDetails]]
        displayNames       <- arbitrary[Map[String, String]]
        loginBox           <- arbitrary[SectionVisibilityState]
        globalLog          <- arbitrary[GlobalLog]
        sequencesOnDisplay <- arbitrary[SequencesOnDisplay]
        appTableStates     <- arbitrary[AppTableStates]
        notification       <- arbitrary[UserNotificationState]
        prompt             <- arbitrary[UserPromptState]
        queues             <- arbitrary[CalibrationQueues]
        progress           <- arbitrary[AllObservationsProgressState]
        filter             <- arbitrary[SessionQueueFilter]
        sound              <- arbitrary[SoundSelection]
        firstLoad          <- arbitrary[Boolean]
      } yield SeqexecUIModel(
        navLocation,
        user,
        displayNames,
        loginBox,
        globalLog,
        sequencesOnDisplay,
        appTableStates,
        notification,
        prompt,
        queues,
        progress,
        filter,
        sound,
        firstLoad
      )
    }

  implicit val seqUIModelCogen: Cogen[SeqexecUIModel] =
    Cogen[
      (
        Pages.SeqexecPages,
        Option[UserDetails],
        Map[String, String],
        SectionVisibilityState,
        GlobalLog,
        SequencesOnDisplay,
        AppTableStates,
        UserNotificationState,
        CalibrationQueues,
        AllObservationsProgressState,
        SessionQueueFilter,
        SoundSelection,
        Boolean
      )
    ]
      .contramap(x =>
        (x.navLocation,
         x.user,
         x.displayNames,
         x.loginBox,
         x.globalLog,
         x.sequencesOnDisplay,
         x.appTableStates,
         x.notification,
         x.queues,
         x.obsProgress,
         x.sessionQueueFilter,
         x.sound,
         x.firstLoad
        )
      )

  implicit val arbSODLocationFocus: Arbitrary[SODLocationFocus] =
    Arbitrary {
      for {
        navLocation        <- arbitrary[Pages.SeqexecPages]
        sequencesOnDisplay <- arbitrary[SequencesOnDisplay]
        clientId           <- arbitrary[Option[ClientId]]
      } yield SODLocationFocus(navLocation, sequencesOnDisplay, clientId)
    }

  implicit val sodLocationFocusogen: Cogen[SODLocationFocus] =
    Cogen[(Pages.SeqexecPages, SequencesOnDisplay, Option[ClientId])]
      .contramap(x => (x.location, x.sod, x.clientId))

  implicit val arbWebSocketsFocus: Arbitrary[WebSocketsFocus] =
    Arbitrary {
      for {
        navLocation          <- arbitrary[Pages.SeqexecPages]
        sequences            <- arbitrary[SequencesQueue[SequenceView]]
        resourceRunRequested <-
          arbitrary[Map[Observation.Id, SortedMap[Resource, ResourceRunOperation]]]
        user                 <- arbitrary[Option[UserDetails]]
        displayNames         <- arbitrary[Map[String, String]]
        clientId             <- arbitrary[Option[ClientId]]
        site                 <- arbitrary[Option[Site]]
        sound                <- arbitrary[SoundSelection]
        serverVer            <- arbitrary[Option[String]]
        guideConf            <- arbitrary[TelescopeGuideConfig]
        acStep               <- arbitrary[AlignAndCalibStep]
      } yield WebSocketsFocus(navLocation,
                              sequences,
                              resourceRunRequested,
                              user,
                              displayNames,
                              clientId,
                              site,
                              sound,
                              serverVer,
                              guideConf,
                              acStep
      )
    }

  implicit val wsfCogen: Cogen[WebSocketsFocus] =
    Cogen[
      (
        Pages.SeqexecPages,
        SequencesQueue[SequenceView],
        Option[UserDetails],
        Map[String, String],
        Option[ClientId],
        Option[Site],
        SoundSelection,
        Option[String],
        TelescopeGuideConfig,
        AlignAndCalibStep
      )
    ]
      .contramap(x =>
        (x.location,
         x.sequences,
         x.user,
         x.displayNames,
         x.clientId,
         x.site,
         x.sound,
         x.serverVersion,
         x.guideConfig,
         x.alignAndCalib
        )
      )

  implicit val arbInitialSyncFocus: Arbitrary[InitialSyncFocus] =
    Arbitrary {
      for {
        navLocation        <- arbitrary[Pages.SeqexecPages]
        sequencesOnDisplay <- arbitrary[SequencesOnDisplay]
        firstLoad          <- arbitrary[Boolean]
        displayNames       <- arbitrary[Map[String, String]]
      } yield InitialSyncFocus(navLocation, sequencesOnDisplay, displayNames, firstLoad)
    }

  implicit val initialSyncFocusCogen: Cogen[InitialSyncFocus] =
    Cogen[(Pages.SeqexecPages, SequencesOnDisplay, Map[String, String], Boolean)].contramap(x =>
      (x.location, x.sod, x.displayNames, x.firstLoad)
    )

  implicit val arbSeqexecAppRootModel: Arbitrary[SeqexecAppRootModel] =
    Arbitrary {
      for {
        sequences <- arbitrary[SequencesQueue[SequenceView]]
        ws        <- arbitrary[WebSocketConnection]
        site      <- arbitrary[Option[Site]]
        clientId  <- arbitrary[Option[ClientId]]
        uiModel   <- arbitrary[SeqexecUIModel]
        version   <- arbitrary[Option[String]]
        guideConf <- arbitrary[TelescopeGuideConfig]
        acStep    <- arbitrary[AlignAndCalibStep]
      } yield SeqexecAppRootModel(sequences,
                                  ws,
                                  site,
                                  clientId,
                                  uiModel,
                                  version,
                                  guideConf,
                                  acStep,
                                  None
      )
    }

  implicit val seqexecAppRootModelCogen: Cogen[SeqexecAppRootModel] =
    Cogen[
      (
        SequencesQueue[SequenceView],
        WebSocketConnection,
        Option[Site],
        Option[ClientId],
        SeqexecUIModel,
        Option[String],
        TelescopeGuideConfig,
        AlignAndCalibStep
      )
    ].contramap { x =>
      (x.sequences,
       x.ws,
       x.site,
       x.clientId,
       x.uiModel,
       x.serverVersion,
       x.guideConfig,
       x.alignAndCalib
      )
    }

  implicit val arbAppTableStates: Arbitrary[AppTableStates] =
    Arbitrary {
      for {
        qt <- arbitrary[TableState[SessionQueueTable.TableColumn]]
        ct <- arbitrary[TableState[StepConfigTable.TableColumn]]
        st <- arbitrary[Map[Observation.Id, TableState[StepsTable.TableColumn]]]
        kt <- arbitrary[Map[QueueId, TableState[CalQueueTable.TableColumn]]]
      } yield AppTableStates(qt, ct, st, kt)
    }

  implicit val appTableStatesCogen: Cogen[AppTableStates] =
    Cogen[
      (
        TableState[SessionQueueTable.TableColumn],
        TableState[StepConfigTable.TableColumn],
        List[(Observation.Id, TableState[StepsTable.TableColumn])],
        List[(QueueId, TableState[CalQueueTable.TableColumn])]
      )
    ]
      .contramap { x =>
        (x.sessionQueueTable, x.stepConfigTable, x.stepsTables.toList, x.queueTables.toList)
      }

}
