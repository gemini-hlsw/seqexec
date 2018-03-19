// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import Model._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Arbitrary._
import java.time.Instant
import cats.implicits._

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

  implicit val clientIdArb: Arbitrary[ClientID] = Arbitrary(Gen.uuid)

  implicit val instArb: Arbitrary[Instant] = Arbitrary {
    for {
      i <- Gen.choose(0L, Long.MaxValue)
    } yield Instant.ofEpochMilli(i)
  }

  implicit val levArb = Arbitrary(Gen.oneOf(ServerLogLevel.INFO, ServerLogLevel.WARN, ServerLogLevel.ERROR))
  implicit val resArb = Arbitrary[Resource](Gen.oneOf(Resource.P1, Resource.OI, Resource.TCS, Resource.Gcal, Resource.Gems, Resource.Altair, Instrument.F2, Instrument.GmosS, Instrument.GmosN, Instrument.GPI, Instrument.GSAOI, Instrument.GNIRS, Instrument.NIRI, Instrument.NIFS))
  implicit val insArb = Arbitrary[Instrument](Gen.oneOf(Instrument.F2, Instrument.GmosS, Instrument.GmosN, Instrument.GPI, Instrument.GSAOI, Instrument.GNIRS, Instrument.NIRI, Instrument.NIFS))

  implicit val actArb = implicitly[Arbitrary[ActionType]]
  implicit val udArb  = implicitly[Arbitrary[UserDetails]]
  implicit val smArb  = implicitly[Arbitrary[SequenceMetadata]]
  implicit val svArb  = implicitly[Arbitrary[SequenceView]]
  implicit val opArb  = implicitly[Arbitrary[Operator]]
  implicit val obArb  = implicitly[Arbitrary[Observer]]
  implicit val spsArb = implicitly[Arbitrary[StepState]]
  implicit val acsArb = implicitly[Arbitrary[ActionStatus]]
  implicit val sqsArb = implicitly[Arbitrary[SequenceState]]
  implicit val ccArb  = Arbitrary[CloudCover](Gen.oneOf(CloudCover.all))
  implicit val wvArb  = Arbitrary[WaterVapor](Gen.oneOf(WaterVapor.all))
  implicit val sbArb  = Arbitrary[SkyBackground](Gen.oneOf(SkyBackground.all))
  implicit val iqArb  = Arbitrary[ImageQuality](Gen.oneOf(ImageQuality.all))

  implicit val conArb = implicitly[Arbitrary[Conditions]]
  // Must define these early on to be used on the events
  implicit val sqiArb = sequencesQueueArb[SequenceId]
  implicit val sqvArb = sequencesQueueArb[SequenceView]
  implicit val snArb  = Arbitrary(Gen.oneOf(SystemName.all))
  implicit val steArb = implicitly[Arbitrary[Step]]
  implicit val stsArb = implicitly[Arbitrary[StandardStep]]
  implicit val styArb = Arbitrary(Gen.oneOf(StepType.all))
  implicit val guiArb = Arbitrary[Guiding](Gen.oneOf(Guiding.Park, Guiding.Guide, Guiding.Freeze))
  implicit val fpmArb = Arbitrary[FPUMode](Gen.oneOf(FPUMode.BuiltIn, FPUMode.Custom))
  implicit val telOffPArb = Arbitrary[TelescopeOffset.P] {
    for {
      d <- Gen.choose(-999.0, 999.0)
    } yield TelescopeOffset.P(d)
  }
  implicit val telOffQArb = Arbitrary[TelescopeOffset.Q] {
    for {
      d <- Gen.choose(-999.0, 999.0)
    } yield TelescopeOffset.Q(d)
  }
  implicit val telOffArb = Arbitrary[TelescopeOffset] {
    for {
      p <- arbitrary[TelescopeOffset.P]
      q <- arbitrary[TelescopeOffset.Q]
    } yield TelescopeOffset(p, q)
  }

  implicit val actCogen: Cogen[ActionType] =
    Cogen[String].contramap(_.productPrefix)

  implicit val snCogen: Cogen[SystemName] =
    Cogen[String].contramap(_.show)

  implicit val resCogen: Cogen[Resource] =
    Cogen[String].contramap(_.show)

  implicit val instCogen: Cogen[Instrument] =
    Cogen[String].contramap(_.show)

  implicit val opCogen: Cogen[Operator] =
    Cogen[String].contramap(_.value)

  implicit val obCogen: Cogen[Observer] =
    Cogen[String].contramap(_.value)

  implicit val stsCogen: Cogen[StepState] =
    Cogen[String].contramap(_.productPrefix)

  implicit val stParams: Cogen[StepConfig] =
    Cogen[String].contramap(_.mkString(","))

  implicit val acsCogen: Cogen[ActionStatus] =
    Cogen[String].contramap(_.productPrefix)

  implicit val stepCogen: Cogen[Step] =
    Cogen[(StepId, Map[SystemName, Map[String, String]], StepState, Boolean, Boolean, Option[dhs.ImageFileId])].contramap(s => (s.id, s.config, s.status, s.breakpoint, s.skip, s.fileId))

  implicit val standardStepCogen: Cogen[StandardStep] =
    Cogen[(StepId, Map[SystemName, Map[String, String]], StepState, Boolean, Boolean, Option[dhs.ImageFileId], List[(Resource, ActionStatus)], ActionStatus)].contramap(s => (s.id, s.config, s.status, s.breakpoint, s.skip, s.fileId, s.configStatus, s.observeStatus))

  implicit val sqsCogen: Cogen[SequenceState] =
    Cogen[String].contramap(_.productPrefix)

  implicit val styCogen: Cogen[StepType] =
    Cogen[String].contramap(_.productPrefix)

  implicit val udCogen: Cogen[UserDetails] =
    Cogen[(String, String)].contramap(u => (u.username, u.displayName))

  implicit val smCogen: Cogen[SequenceMetadata] =
    Cogen[(Instrument, Option[Observer], String)].contramap(s => (s.instrument, s.observer, s.name))

  implicit val svCogen: Cogen[SequenceView] =
    Cogen[(SequenceId, SequenceMetadata, SequenceState, List[Step], Option[Int])].contramap(s => (s.id, s.metadata, s.status, s.steps, s.willStopIn))

  implicit def sqCogen[A: Cogen]: Cogen[SequencesQueue[A]] =
    Cogen[(Conditions, Option[Operator], List[A])].contramap(s => (s.conditions, s.operator, s.queue))

  implicit val offPCogen: Cogen[TelescopeOffset.P] =
    Cogen[Double].contramap(_.value)

  implicit val offQCogen: Cogen[TelescopeOffset.Q] =
    Cogen[Double].contramap(_.value)

  implicit val offCogen: Cogen[TelescopeOffset] =
    Cogen[(TelescopeOffset.P, TelescopeOffset.Q)].contramap(o => (o.p, o.q))

  implicit val guiCogen: Cogen[Guiding] =
    Cogen[String].contramap(_.productPrefix)

  implicit val fpuCogen: Cogen[FPUMode] =
    Cogen[String].contramap(_.productPrefix)

  implicit val ccCogen: Cogen[CloudCover] =
    Cogen[String].contramap(_.productPrefix)

  implicit val wvCogen: Cogen[WaterVapor] =
    Cogen[String].contramap(_.productPrefix)

  implicit val sbCogen: Cogen[SkyBackground] =
    Cogen[String].contramap(_.productPrefix)

  implicit val iqCogen: Cogen[ImageQuality] =
    Cogen[String].contramap(_.productPrefix)

  implicit val conCogen: Cogen[Conditions] =
    Cogen[(CloudCover, ImageQuality, SkyBackground, WaterVapor)].contramap(c => (c.cc, c.iq, c.sb, c.wv))

  implicit val cidCogen: Cogen[ClientID] =
    Cogen[String].contramap(_.toString)

}
