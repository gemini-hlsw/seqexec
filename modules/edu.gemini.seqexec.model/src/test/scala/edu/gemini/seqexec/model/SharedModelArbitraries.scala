// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import Model._
import events.{SeqexecEvent, SeqexecModelUpdate}
import events.SeqexecEvent._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._
import java.time.Instant

// Keep the arbitraries in a separate trait to improve caching
@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object SharedModelArbitraries {

  import org.scalacheck.ScalacheckShapeless._
  private val maxListSize = 2

  // N.B. We don't want to auto derive this to limit the size of the lists for performance reasons
  implicit def sequencesQueueArb[A](implicit arb: Arbitrary[A]): Arbitrary[SequencesQueue[A]] = Arbitrary {
    for {
      b <- Gen.listOfN[A](maxListSize, arb.arbitrary)
      // We are already testing serialization of conditions and Strings
      // Let's reduce the test space by only testing the list of items
    } yield SequencesQueue(Conditions.default, Some(Operator("operator")), b)
  }

  implicit val instArb: Arbitrary[Instant] = Arbitrary {
    for {
      i <- Gen.choose(0L, Long.MaxValue)
    } yield Instant.ofEpochMilli(i)
  }
  implicit val levArb = Arbitrary(Gen.oneOf(ServerLogLevel.INFO, ServerLogLevel.WARN, ServerLogLevel.ERROR))

  implicit val udArb  = implicitly[Arbitrary[UserDetails]]
  implicit val svArb  = implicitly[Arbitrary[SequenceView]]
  // Must define these early on to be used on the events
  implicit val sqiArb = sequencesQueueArb[SequenceId]
  implicit val sqvArb = sequencesQueueArb[SequenceView]
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
  implicit val iqArb  = implicitly[Arbitrary[ImageQuality]]
  implicit val wvArb  = implicitly[Arbitrary[WaterVapor]]
  implicit val sbArb  = implicitly[Arbitrary[SkyBackground]]
  implicit val ccArb  = implicitly[Arbitrary[CloudCover]]
  implicit val conArb = implicitly[Arbitrary[Conditions]]
  implicit val seArb  = implicitly[Arbitrary[SeqexecEvent]]
  implicit val smuArb = implicitly[Arbitrary[SeqexecModelUpdate]]
  implicit val serArb = implicitly[Arbitrary[SequenceError]]
  implicit val sspArb = implicitly[Arbitrary[SequencePaused]]
  implicit val sepArb = implicitly[Arbitrary[ExposurePaused]]
  implicit val snArb  = Arbitrary(Gen.oneOf(SystemName.all))
  implicit val steArb = implicitly[Arbitrary[Step]]
  implicit val stsArb = implicitly[Arbitrary[StandardStep]]
  implicit val styArb = Arbitrary(Gen.oneOf(StepType.all))
  implicit val ofpArb = implicitly[Arbitrary[TelescopeOffset.P]]
  implicit val ofqArb = implicitly[Arbitrary[TelescopeOffset.Q]]
  implicit val guiArb = Arbitrary[Guiding](Gen.oneOf(Guiding.Park, Guiding.Guide, Guiding.Freeze))
  implicit val fpmArb = Arbitrary[FPUMode](Gen.oneOf(FPUMode.BuiltIn, FPUMode.Custom))
}
