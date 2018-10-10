// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import events._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import gem.Observation
import gem.arb.ArbTime
import java.time.Instant
import seqexec.model.enum._
import seqexec.model.enum.QueueManipulationOp._
import seqexec.model.SeqexecModelArbitraries._

trait SequenceEventsArbitraries extends ArbTime {

  implicit val coeArb = Arbitrary[ConnectionOpenEvent] {
    for {
      u  <- arbitrary[Option[UserDetails]]
      id <- arbitrary[ClientId]
    } yield ConnectionOpenEvent(u, id)
  }
  implicit val sseArb = Arbitrary[SequenceStart] {
    arbitrary[SequencesQueue[SequenceView]].map(SequenceStart.apply)
  }
  implicit val seeArb = Arbitrary[StepExecuted] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield StepExecuted(i, s)
  }
  implicit val sceArb = Arbitrary[SequenceCompleted] {
    arbitrary[SequencesQueue[SequenceView]].map(SequenceCompleted.apply)
  }
  implicit val sleArb = Arbitrary[SequenceLoaded] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequenceLoaded(i, s)
  }
  implicit val sueArb = Arbitrary[SequenceUnloaded] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequenceUnloaded(i, s)
  }
  implicit val sbeArb = Arbitrary[StepBreakpointChanged] {
    arbitrary[SequencesQueue[SequenceView]].map(StepBreakpointChanged.apply)
  }
  implicit val smeArb = Arbitrary[StepSkipMarkChanged] {
    arbitrary[SequencesQueue[SequenceView]].map(StepSkipMarkChanged.apply)
  }
  implicit val speArb = Arbitrary[SequencePauseRequested] {
    arbitrary[SequencesQueue[SequenceView]].map(SequencePauseRequested.apply)
  }
  implicit val spcArb = Arbitrary[SequencePauseCanceled] {
    arbitrary[SequencesQueue[SequenceView]].map(SequencePauseCanceled.apply)
  }
  implicit val srfArb = Arbitrary[SequenceRefreshed] {
    for {
      s <- arbitrary[SequencesQueue[SequenceView]]
      c <- arbitrary[ClientId]
    } yield SequenceRefreshed(s, c)
  }
  implicit val asrArb = Arbitrary[ActionStopRequested] {
    arbitrary[SequencesQueue[SequenceView]].map(ActionStopRequested.apply)
  }
  implicit val nlmArb = Arbitrary[NewLogMessage] {
    arbitrary[String].map(NewLogMessage.apply)
  }
  implicit val slmArb = Arbitrary[ServerLogMessage] {
    for {
      l <- arbitrary[ServerLogLevel]
      t <- arbitrary[Instant]
      m <- Gen.alphaStr
    } yield ServerLogMessage(l, t, m)
  }
  implicit val neArb = Arbitrary[NullEvent.type] { NullEvent }
  implicit val opArb = Arbitrary[OperatorUpdated] {
    arbitrary[SequencesQueue[SequenceView]].map(OperatorUpdated.apply)
  }
  implicit val obArb = Arbitrary[ObserverUpdated] {
    arbitrary[SequencesQueue[SequenceView]].map(ObserverUpdated.apply)
  }
  implicit val cuArb = Arbitrary[ConditionsUpdated] {
    arbitrary[SequencesQueue[SequenceView]].map(ConditionsUpdated.apply)
  }
  implicit val qmArb = Arbitrary[QueueManipulationOp] {
    for {
      q <- arbitrary[QueueId]
      i <- arbitrary[List[Observation.Id]]
      m <- Gen.oneOf(Moved(q), Started(q), Stopped(q), Clear(q), AddedSeqs(q, i), RemovedSeqs(q, i))
    } yield m
  }
  implicit val quArb = Arbitrary[QueueUpdated] {
    for {
      o <- arbitrary[QueueManipulationOp]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield QueueUpdated(o, s)
  }
  implicit val suArb = Arbitrary[LoadSequenceUpdated] {
    for {
      i <- arbitrary[Instrument]
      o <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
      c <- arbitrary[ClientId]
    } yield LoadSequenceUpdated(i, o, s, c)
  }
  implicit val clsArb = Arbitrary[ClearLoadedSequencesUpdated] {
    arbitrary[SequencesQueue[SequenceView]]
      .map(ClearLoadedSequencesUpdated.apply)
  }
  implicit val serArb = Arbitrary[SequenceError] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequenceError(i, s)
  }

  implicit val supArb = Arbitrary[SequenceUpdated] {
    arbitrary[SequencesQueue[SequenceView]].map(SequenceUpdated.apply)
  }

  implicit val sspArb = Arbitrary[SequencePaused] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequencePaused(i, s)
  }

  implicit val sepArb = Arbitrary[ExposurePaused] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield ExposurePaused(i, s)
  }

  implicit val fidArb = Arbitrary[FileIdStepExecuted] {
    for {
      i <- arbitrary[String]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield FileIdStepExecuted(i, s)
  }

  implicit val unArb = Arbitrary[UserNotification] {
    for {
      i <- arbitrary[Notification]
      c <- arbitrary[ClientId]
    } yield UserNotification(i, c)
  }

  implicit val smuArb = Arbitrary[SeqexecModelUpdate] {
    Gen.oneOf[SeqexecModelUpdate](
      arbitrary[SequenceStart],
      arbitrary[StepExecuted],
      arbitrary[FileIdStepExecuted],
      arbitrary[SequenceCompleted],
      arbitrary[SequenceLoaded],
      arbitrary[SequenceUnloaded],
      arbitrary[StepBreakpointChanged],
      arbitrary[OperatorUpdated],
      arbitrary[ObserverUpdated],
      arbitrary[ConditionsUpdated],
      arbitrary[StepSkipMarkChanged],
      arbitrary[SequencePauseRequested],
      arbitrary[SequencePauseCanceled],
      arbitrary[SequenceRefreshed],
      arbitrary[ActionStopRequested],
      arbitrary[SequenceUpdated],
      arbitrary[SequencePaused],
      arbitrary[ExposurePaused],
      arbitrary[LoadSequenceUpdated],
      arbitrary[ClearLoadedSequencesUpdated],
      arbitrary[QueueUpdated],
      arbitrary[SequenceError]
    )
  }
  implicit val seArb = Arbitrary[SeqexecEvent] {
    Gen.oneOf[SeqexecEvent](
      arbitrary[SeqexecModelUpdate],
      arbitrary[ConnectionOpenEvent],
      arbitrary[NewLogMessage],
      arbitrary[ServerLogMessage],
      arbitrary[UserNotification],
      arbitrary[NullEvent.type]
    )
  }

  implicit val coeCogen: Cogen[ConnectionOpenEvent] =
    Cogen[Option[UserDetails]].contramap(_.u)

  implicit val smuCogen: Cogen[SeqexecModelUpdate] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sseCogen: Cogen[SequenceStart] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val seeCogen: Cogen[StepExecuted] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val fidCogen: Cogen[FileIdStepExecuted] =
    Cogen[(dhs.ImageFileId, SequencesQueue[SequenceView])]
      .contramap(x => (x.fileId, x.view))

  implicit val sceCogen: Cogen[SequenceCompleted] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sleCogen: Cogen[SequenceLoaded] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sueCogen: Cogen[SequenceUnloaded] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val smeCogen: Cogen[StepSkipMarkChanged] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val opCogen: Cogen[OperatorUpdated] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val obCogen: Cogen[ObserverUpdated] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val cuCogen: Cogen[ConditionsUpdated] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sbeCogen: Cogen[StepBreakpointChanged] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val speCogen: Cogen[SequencePauseRequested] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val spcCogen: Cogen[SequencePauseCanceled] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val srfCogen: Cogen[SequenceRefreshed] =
    Cogen[(ClientId, SequencesQueue[SequenceView])]
      .contramap(x => (x.clientId, x.view))

  implicit val asrCogen: Cogen[ActionStopRequested] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val supCogen: Cogen[SequenceUpdated] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sspCogen: Cogen[SequencePaused] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])]
      .contramap(x => (x.obsId, x.view))

  implicit val sepCogen: Cogen[ExposurePaused] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])]
      .contramap(x => (x.obsId, x.view))

  implicit val serCogen: Cogen[SequenceError] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])]
      .contramap(x => (x.obsId, x.view))

  implicit val slmCogen: Cogen[ServerLogMessage] =
    Cogen[(ServerLogLevel, Instant, String)]
      .contramap(x => (x.level, x.timestamp, x.msg))

  implicit val nlmCogen: Cogen[NewLogMessage] =
    Cogen[String].contramap(_.msg)

  implicit val unCogen: Cogen[UserNotification] =
    Cogen[(Notification, ClientId)].contramap(x => (x.memo, x.clientId))
}

object SequenceEventsArbitraries extends SequenceEventsArbitraries
