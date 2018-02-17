// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import Model._
import events.{SeqexecEvent, SeqexecModelUpdate}
import events.SeqexecEvent._
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
  implicit val asrArb = implicitly[Arbitrary[ActionStopRequested]]
  implicit val slmArb = implicitly[Arbitrary[ServerLogMessage]]
  implicit val neArb  = implicitly[Arbitrary[NullEvent.type]]
  implicit val opArb  = implicitly[Arbitrary[OperatorUpdated]]
  implicit val obArb  = implicitly[Arbitrary[ObserverUpdated]]
  implicit val seArb  = implicitly[Arbitrary[SeqexecEvent]]
  implicit val smuArb = implicitly[Arbitrary[SeqexecModelUpdate]]
  implicit val serArb = implicitly[Arbitrary[SequenceError]]
  implicit val sspArb = implicitly[Arbitrary[SequencePaused]]
  implicit val sepArb = implicitly[Arbitrary[ExposurePaused]]

  implicit val coeCogen: Cogen[ConnectionOpenEvent] =
    Cogen[Option[UserDetails]].contramap(_.u)

  implicit val smuCogen: Cogen[SeqexecModelUpdate] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val sseCogen: Cogen[SequenceStart] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)

  implicit val seeCogen: Cogen[StepExecuted] =
    Cogen[SequencesQueue[SequenceView]].contramap(_.view)
}
