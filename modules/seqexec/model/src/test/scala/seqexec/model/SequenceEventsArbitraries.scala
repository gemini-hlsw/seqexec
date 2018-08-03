// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import events._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import gem.Observation
import java.time.Instant
import seqexec.model.enum._

// Keep the arbitraries in a separate trait to improve caching
@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequenceEventsArbitraries {
  import SharedModelArbitraries._

  implicit val coeArb = Arbitrary[ConnectionOpenEvent] {
    for {
      u <- arbitrary[Option[UserDetails]]
      id <- arbitrary[ClientID]
    } yield ConnectionOpenEvent(u, id)}
  implicit val sseArb = Arbitrary[SequenceStart] { arbitrary[SequencesQueue[SequenceView]].map(SequenceStart.apply) }
  implicit val seeArb = Arbitrary[StepExecuted] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield StepExecuted(i, s)
  }
  implicit val sceArb = Arbitrary[SequenceCompleted] { arbitrary[SequencesQueue[SequenceView]].map(SequenceCompleted.apply) }
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
  implicit val sbeArb = Arbitrary[StepBreakpointChanged] { arbitrary[SequencesQueue[SequenceView]].map(StepBreakpointChanged.apply) }
  implicit val smeArb = Arbitrary[StepSkipMarkChanged] { arbitrary[SequencesQueue[SequenceView]].map(StepSkipMarkChanged.apply) }
  implicit val speArb = Arbitrary[SequencePauseRequested] { arbitrary[SequencesQueue[SequenceView]].map(SequencePauseRequested.apply) }
  implicit val spcArb = Arbitrary[SequencePauseCanceled] { arbitrary[SequencesQueue[SequenceView]].map(SequencePauseCanceled.apply) }
  implicit val srfArb = Arbitrary[SequenceRefreshed] {
    for {
      s <- arbitrary[SequencesQueue[SequenceView]]
      c <- arbitrary[ClientID]
    } yield SequenceRefreshed(s, c)
  }
  implicit val asrArb = Arbitrary[ActionStopRequested] { arbitrary[SequencesQueue[SequenceView]].map(ActionStopRequested.apply) }
  implicit val rcbArb = Arbitrary[ResourcesBusy] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
      c <- arbitrary[ClientID]
    } yield ResourcesBusy(i, s, c)
  }
  implicit val nlmArb = Arbitrary[NewLogMessage] { arbitrary[String].map(NewLogMessage.apply) }
  implicit val slmArb = Arbitrary[ServerLogMessage] {
    for {
      l <- arbitrary[ServerLogLevel]
      t <- arbitrary[Instant]
      m <- arbitrary[String]
    } yield ServerLogMessage(l, t, m)
  }
  implicit val neArb  = Arbitrary[NullEvent.type] { NullEvent }
  implicit val opArb  = Arbitrary[OperatorUpdated] { arbitrary[SequencesQueue[SequenceView]].map(OperatorUpdated.apply) }
  implicit val obArb  = Arbitrary[ObserverUpdated] { arbitrary[SequencesQueue[SequenceView]].map(ObserverUpdated.apply) }
  implicit val cuArb  = Arbitrary[ConditionsUpdated] { arbitrary[SequencesQueue[SequenceView]].map(ConditionsUpdated.apply) }
  implicit val suArb  = Arbitrary[LoadSequenceUpdated] {
    for {
      i <- arbitrary[Instrument]
      o <- arbitrary[Observation.Id]
    } yield LoadSequenceUpdated(i, o)
  }
  implicit val clsArb = Arbitrary[ClearLoadedSequencesUpdated.type] { ClearLoadedSequencesUpdated }
  implicit val serArb = Arbitrary[SequenceError] {
    for {
      i <- arbitrary[Observation.Id]
      s <- arbitrary[SequencesQueue[SequenceView]]
    } yield SequenceError(i, s)
  }

  implicit val supArb = Arbitrary[SequenceUpdated] { arbitrary[SequencesQueue[SequenceView]].map(SequenceUpdated.apply) }

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
        arbitrary[ResourcesBusy],
        arbitrary[SequenceUpdated],
        arbitrary[SequencePaused],
        arbitrary[ExposurePaused],
        arbitrary[SequenceError]
    )
  }
  implicit val seArb  = Arbitrary[SeqexecEvent] {
    Gen.oneOf[SeqexecEvent](
      arbitrary[SeqexecModelUpdate],
      arbitrary[ConnectionOpenEvent],
      arbitrary[NewLogMessage],
      arbitrary[ServerLogMessage],
      arbitrary[LoadSequenceUpdated],
      arbitrary[ClearLoadedSequencesUpdated.type],
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
    Cogen[(dhs.ImageFileId, SequencesQueue[SequenceView])].contramap(x => (x.fileId, x.view))

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
    Cogen[(ClientID, SequencesQueue[SequenceView])].contramap(x => (x.clientId, x.view))

  implicit val asrCogen: Cogen[ActionStopRequested] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val rcbCogen: Cogen[ResourcesBusy] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView], ClientID)].contramap(x => (x.obsId, x.view, x.clientId))

  implicit val supCogen: Cogen[SequenceUpdated] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sspCogen: Cogen[SequencePaused] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])].contramap(x => (x.obsId, x.view))

  implicit val sepCogen: Cogen[ExposurePaused] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])].contramap(x => (x.obsId, x.view))

  implicit val serCogen: Cogen[SequenceError] =
    Cogen[(Observation.Id, SequencesQueue[SequenceView])].contramap(x => (x.obsId, x.view))

  implicit val nlmCogen: Cogen[NewLogMessage] =
    Cogen[String].contramap(_.msg)
}
