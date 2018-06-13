// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import Model._
import events._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Arbitrary._

// Keep the arbitraries in a separate trait to improve caching
@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SequenceEventsArbitraries {
  import SharedModelArbitraries._
  import org.scalacheck.ScalacheckShapeless._

  implicit val coeArb = implicitly[Arbitrary[ConnectionOpenEvent]]
  implicit val sseArb = implicitly[Arbitrary[SequenceStart]]
  implicit val seeArb = implicitly[Arbitrary[StepExecuted]]
  implicit val sceArb = implicitly[Arbitrary[SequenceCompleted]]
  implicit val sleArb = implicitly[Arbitrary[SequenceLoaded]]
  implicit val sueArb = implicitly[Arbitrary[SequenceUnloaded]]
  implicit val sbeArb = implicitly[Arbitrary[StepBreakpointChanged]]
  implicit val smeArb = implicitly[Arbitrary[StepSkipMarkChanged]]
  implicit val speArb = implicitly[Arbitrary[SequencePauseRequested]]
  implicit val spcArb = implicitly[Arbitrary[SequencePauseCanceled]]
  implicit val srfArb = implicitly[Arbitrary[SequenceRefreshed]]
  implicit val asrArb = implicitly[Arbitrary[ActionStopRequested]]
  implicit val rcbArb = implicitly[Arbitrary[ResourcesBusy]]
  implicit val nlmArb = implicitly[Arbitrary[NewLogMessage]]
  implicit val slmArb = implicitly[Arbitrary[ServerLogMessage]]
  implicit val neArb  = implicitly[Arbitrary[NullEvent.type]]
  implicit val opArb  = implicitly[Arbitrary[OperatorUpdated]]
  implicit val obArb  = implicitly[Arbitrary[ObserverUpdated]]
  implicit val cuArb  = implicitly[Arbitrary[ConditionsUpdated]]
  implicit val seArb  = implicitly[Arbitrary[SeqexecEvent]]
  implicit val smuArb = implicitly[Arbitrary[SeqexecModelUpdate]]
  implicit val serArb = implicitly[Arbitrary[SequenceError]]
  implicit val supArb = implicitly[Arbitrary[SequenceUpdated]]
  implicit val sspArb = implicitly[Arbitrary[SequencePaused]]
  implicit val sepArb = implicitly[Arbitrary[ExposurePaused]]
  implicit val fidArb = implicitly[Arbitrary[FileIdStepExecuted]]

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
    Cogen[(SequenceId, SequencesQueue[SequenceView], ClientID)].contramap(x => (x.obsId, x.view, x.clientId))

  implicit val supCogen: Cogen[SequenceUpdated] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sspCogen: Cogen[SequencePaused] =
    Cogen[(SequenceId, SequencesQueue[SequenceView])].contramap(x => (x.obsId, x.view))

  implicit val sepCogen: Cogen[ExposurePaused] =
    Cogen[(SequenceId, SequencesQueue[SequenceView])].contramap(x => (x.obsId, x.view))

  implicit val serCogen: Cogen[SequenceError] =
    Cogen[(SequenceId, SequencesQueue[SequenceView])].contramap(x => (x.obsId, x.view))

  implicit val nlmCogen: Cogen[NewLogMessage] =
    Cogen[String].contramap(_.msg)
}
