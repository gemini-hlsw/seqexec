package edu.gemini.seqexec.model

import Model._
import Model.SeqexecEvent._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary._

// Keep the arbitraries in a separate trait to improve caching
object SharedModelArbitraries {
  import org.scalacheck.Shapeless._
  val maxListSize = 2

  // N.B. We don't want to auto derive this to limit the size of the lists for performance reasons
  def sequencesQueueArb[A](implicit arb: Arbitrary[A]): Arbitrary[SequencesQueue[A]] = Arbitrary {
    for {
      b <- Gen.listOfN[A](maxListSize, arb.arbitrary)
      // We are already testing serialization of conditions and Strings
      // Let's reduce the test space by only testing the list of items
    } yield SequencesQueue(Conditions.default, Some("operator"), b)
  }

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
  implicit val lmArb  = implicitly[Arbitrary[NewLogMessage]]
  implicit val neArb  = implicitly[Arbitrary[NullEvent.type]]
  implicit val opArb  = implicitly[Arbitrary[OperatorUpdated]]
  implicit val obArb  = implicitly[Arbitrary[ObserverUpdated]]
  implicit val iqArb  = implicitly[Arbitrary[ImageQuality]]
  implicit val wvArb  = implicitly[Arbitrary[WaterVapor]]
  implicit val sbArb  = implicitly[Arbitrary[SkyBackground]]
  implicit val ccArb  = implicitly[Arbitrary[CloudCover]]
  implicit val conArb = implicitly[Arbitrary[Conditions]]
  implicit val seArb  = implicitly[Arbitrary[SeqexecEvent]]
}

