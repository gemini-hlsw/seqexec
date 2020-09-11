// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import events._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import gem.Observation
import gem.arb.ArbEnumerated.{arbEnumerated => oldArbEnumerated}
import gem.arb.ArbEnumerated.{cogEnumerated => oldCogEnumerated}
import lucuma.core.util.arb.ArbEnumerated._
import gsp.math.arb.ArbTime._
import java.time.Instant
import seqexec.model.enum._
import seqexec.model.dhs._
import seqexec.model.QueueManipulationOp._
import seqexec.model.SeqexecModelArbitraries._
import seqexec.model.arb.all._

trait SequenceEventsArbitraries {

  implicit val gcuArb = Arbitrary[GuideConfigUpdate] {
    arbitrary[TelescopeGuideConfig].map(GuideConfigUpdate.apply)
  }

  implicit val coeArb = Arbitrary[ConnectionOpenEvent] {
    for {
      u  <- arbitrary[Option[UserDetails]]
      id <- arbitrary[ClientId]
      v  <- arbitrary[String]
    } yield ConnectionOpenEvent(u, id, v)
  }

  implicit val sseArb = Arbitrary[SequenceStart] {
    for {
      id <- arbitrary[Observation.Id]
      si <- arbitrary[StepId]
      sv <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequenceStart(id, si, sv)
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
  implicit val sstArb = Arbitrary[SequenceStopped] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequenceStopped(i, s)
  }
  implicit val ssaArb = Arbitrary[SequenceAborted] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequenceAborted(i, s)
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
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequencePauseCanceled(i, s)
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
      r <- arbitrary[List[Int]]
      c <- arbitrary[ClientId]
      o <- arbitrary[Observation.Id]
      p <- arbitrary[Int]
      m <- Gen.oneOf(Moved(q, c, o, p), Started(q), Stopped(q), Clear(q), AddedSeqs(q, i), RemovedSeqs(q, i, r))
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
      i <- arbitrary[ImageFileId]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield FileIdStepExecuted(i, s)
  }

  implicit val unArb = Arbitrary[UserNotification] {
    for {
      i <- arbitrary[Notification]
      c <- arbitrary[ClientId]
    } yield UserNotification(i, c)
  }

  implicit val unpArb = Arbitrary[UserPromptNotification] {
    for {
      i <- arbitrary[UserPrompt]
      c <- arbitrary[ClientId]
    } yield UserPromptNotification(i, c)
  }

  implicit val oprArb = Arbitrary[ObservationProgressEvent] {
    for {
      p <- arbitrary[ObservationProgress]
    } yield ObservationProgressEvent(p)
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

  implicit val acpArb = Arbitrary[AlignAndCalibEvent] {
    for {
      p <- Gen.posNum[Int]
    } yield AlignAndCalibEvent(p)
  }

  implicit val seArb = Arbitrary[SeqexecEvent] {
    Gen.oneOf[SeqexecEvent](
      arbitrary[SeqexecModelUpdate],
      arbitrary[ConnectionOpenEvent],
      arbitrary[ServerLogMessage],
      arbitrary[UserNotification],
      arbitrary[UserPromptNotification],
      arbitrary[ObservationProgressEvent],
      arbitrary[AlignAndCalibEvent],
      arbitrary[NullEvent.type]
    )
  }

  implicit val coeCogen: Cogen[ConnectionOpenEvent] =
    Cogen[(Option[UserDetails], ClientId, String)]
      .contramap(x => (x.userDetails, x.clientId, x.serverVersion))

  implicit val smuCogen: Cogen[SeqexecModelUpdate] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val gcuCogen: Cogen[GuideConfigUpdate] =
    Cogen[TelescopeGuideConfig].contramap(_.telescope)

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

  implicit val sstCogen: Cogen[SequenceStopped] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])]
      .contramap(x => (x.obsId, x.view))

  implicit val ssaCogen: Cogen[SequenceAborted] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])]
      .contramap(x => (x.obsId, x.view))

  implicit val slmCogen: Cogen[ServerLogMessage] =
    Cogen[(ServerLogLevel, Instant, String)]
      .contramap(x => (x.level, x.timestamp, x.msg))

  implicit val upnCogen: Cogen[UserPromptNotification] =
    Cogen[(UserPrompt, ClientId)].contramap(x => (x.prompt, x.clientId))

  implicit val unCogen: Cogen[UserNotification] =
    Cogen[(Notification, ClientId)].contramap(x => (x.memo, x.clientId))

  implicit val oprCogen: Cogen[ObservationProgressEvent] =
    Cogen[Progress].contramap(_.progress)

  implicit val acpCogen: Cogen[AlignAndCalibEvent] =
    Cogen[Int].contramap(_.step)

}

object SequenceEventsArbitraries extends SequenceEventsArbitraries
