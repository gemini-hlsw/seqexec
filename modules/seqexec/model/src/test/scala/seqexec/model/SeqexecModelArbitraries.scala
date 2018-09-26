// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import cats.implicits._
import gem.Observation
import gem.arb.ArbObservation
import seqexec.model.enum._

trait SeqexecModelArbitraries extends ArbObservation {

  private val maxListSize = 2

  // N.B. We don't want to auto derive this to limit the size of the lists for performance reasons
  implicit def sequencesQueueArb[A](implicit arb: Arbitrary[A]): Arbitrary[SequencesQueue[A]] = Arbitrary {
    for {
      b <- Gen.listOfN[A](maxListSize, arb.arbitrary)
      c <- arbitrary[Conditions]
      o <- arbitrary[Option[Operator]]
      // We are already testing serialization of conditions and Strings
      // Let's reduce the test space by only testing the list of items
    } yield SequencesQueue(Map.empty, c, o, Map.empty, b)
  }

  implicit val clientIdArb: Arbitrary[ClientID] = Arbitrary(Gen.uuid)

  implicit val levArb = Arbitrary[ServerLogLevel](Gen.oneOf(ServerLogLevel.INFO, ServerLogLevel.WARN, ServerLogLevel.ERROR))
  implicit val resArb = Arbitrary[Resource](Gen.oneOf(Resource.P1, Resource.OI, Resource.TCS, Resource.Gcal, Resource.Gems, Resource.Altair, Instrument.F2, Instrument.GmosS, Instrument.GmosN, Instrument.GPI, Instrument.GSAOI, Instrument.GNIRS, Instrument.NIRI, Instrument.NIFS))
  implicit val insArb = Arbitrary[Instrument](Gen.oneOf(Instrument.F2, Instrument.GmosS, Instrument.GmosN, Instrument.GPI, Instrument.GSAOI, Instrument.GNIRS, Instrument.NIRI, Instrument.NIFS))

  implicit val executionQueueArb: Arbitrary[ExecutionQueue] = Arbitrary {
    for {
      n <- arbitrary[String]
      s <- arbitrary[BatchCommandState]
      q <- arbitrary[List[Observation.Id]]
    } yield ExecutionQueue(n, s, q)
  }

  implicit val actArb = Arbitrary[ActionType] {
    for {
      c <- arbitrary[Resource].map(ActionType.Configure.apply)
      a <- Gen.oneOf(ActionType.Observe, ActionType.Undefined)
      b <- Gen.oneOf(c, a)
    } yield b
  }

  implicit val udArb  = Arbitrary[UserDetails] {
    for {
      u <- arbitrary[String]
      n <- arbitrary[String]
    } yield UserDetails(u, n)
  }

  implicit val obArb  = Arbitrary[Observer] { Gen.alphaStr.map(Observer.apply) }
  implicit val smArb  = Arbitrary[SequenceMetadata] {
    for {
      i <- arbitrary[Instrument]
      o <- arbitrary[Option[Observer]]
      n <- arbitrary[String]
    } yield SequenceMetadata(i, o, n)
  }

  implicit val opArb  = Arbitrary[Operator] { Gen.alphaStr.map(Operator.apply) }
  implicit val spsArb = Arbitrary[StepState] {
    for {
      v1 <- Gen.oneOf(StepState.Pending, StepState.Completed, StepState.Skipped, StepState.Running, StepState.Paused)
      v2 <- Gen.alphaStr.map(StepState.Failed.apply)
      r  <- Gen.oneOf(v1, v2)
    } yield r
  }

  implicit val acsArb = Arbitrary[ActionStatus](Gen.oneOf(ActionStatus.Pending, ActionStatus.Completed, ActionStatus.Running, ActionStatus.Paused, ActionStatus.Failed))

  implicit val sqrArb = Arbitrary[SequenceState.Running] {
    for {
      u <- arbitrary[Boolean]
      i <- arbitrary[Boolean]
    } yield SequenceState.Running(u, i)
  }

  implicit val sqsArb = Arbitrary[SequenceState] {
    for {
      f <- Gen.oneOf(SequenceState.Completed, SequenceState.Idle, SequenceState.Stopped)
      r <- arbitrary[SequenceState.Running]
      a <- arbitrary[String].map(SequenceState.Failed.apply)
      s <- Gen.oneOf(f, r, a)
    } yield s
  }
  implicit val ccArb  = Arbitrary[CloudCover](Gen.oneOf(CloudCover.all))
  implicit val wvArb  = Arbitrary[WaterVapor](Gen.oneOf(WaterVapor.all))
  implicit val sbArb  = Arbitrary[SkyBackground](Gen.oneOf(SkyBackground.all))
  implicit val iqArb  = Arbitrary[ImageQuality](Gen.oneOf(ImageQuality.all))

  implicit val conArb = Arbitrary[Conditions] {
    for {
      cc <- arbitrary[CloudCover]
      iq <- arbitrary[ImageQuality]
      sb <- arbitrary[SkyBackground]
      wv <- arbitrary[WaterVapor]
    } yield Conditions(cc, iq, sb, wv)
  }

  implicit val snArb  = Arbitrary(Gen.oneOf(SystemName.all))

  def asciiStr: Gen[String] =
    Gen.listOf(Gen.alphaChar).map(_.mkString)

  val stepItemG: Gen[(String, String)] =
    for {
      a <- asciiStr
      b <- asciiStr
    } yield (a, b)

  val parametersGen: Gen[Parameters] = Gen.chooseNum(0, 10).flatMap(s => Gen.mapOfN[String, String](s, stepItemG))

  val stepConfigG: Gen[(SystemName, Parameters)] =
    for {
      a <- arbitrary[SystemName]
      b <- parametersGen
    } yield (a, b)

  val stepConfigGen: Gen[StepConfig] = Gen.chooseNum(0, 3).flatMap(s => Gen.mapOfN[SystemName, Parameters](s, stepConfigG))
  implicit val steArb = Arbitrary[Step] {
    for {
      id <- arbitrary[StepId]
      c  <- stepConfigGen
      s  <- arbitrary[StepState]
      b  <- arbitrary[Boolean]
      k  <- arbitrary[Boolean]
      f  <- arbitrary[Option[dhs.ImageFileId]]
    } yield new StandardStep(id = id, config = c, status = s, breakpoint = b, skip = k, fileId = f, configStatus = Nil, observeStatus = ActionStatus.Pending)
  }

  implicit val stsArb = Arbitrary[StandardStep] {
    for {
      id <- arbitrary[StepId]
      c  <- stepConfigGen
      s  <- arbitrary[StepState]
      b  <- arbitrary[Boolean]
      k  <- arbitrary[Boolean]
      f  <- arbitrary[Option[dhs.ImageFileId]]
      cs <- arbitrary[List[(Resource, ActionStatus)]]
      os <- arbitrary[ActionStatus]
    } yield new StandardStep(id = id, config = c, status = s, breakpoint = b, skip = k, fileId = f, configStatus = cs, observeStatus = os)
  }

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
  implicit val svArb  = Arbitrary[SequenceView] {
    for {
      id <- arbitrary[Observation.Id]
      m  <- arbitrary[SequenceMetadata]
      s  <- arbitrary[SequenceState]
      t  <- arbitrary[List[Step]]
      i  <- arbitrary[Option[Int]]
    } yield SequenceView(id, m, s, t, i)
  }
  implicit val sqvArb = sequencesQueueArb[SequenceView]

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
    Cogen[(Observation.Id, SequenceMetadata, SequenceState, List[Step], Option[Int])].contramap(s => (s.id, s.metadata, s.status, s.steps, s.willStopIn))

  implicit def sqCogen[A: Cogen]: Cogen[SequencesQueue[A]] =
    Cogen[(Conditions, Option[Operator], List[A])].contramap(s => (s.conditions, s.operator, s.sessionQueue))

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

  implicit val levCogen: Cogen[ServerLogLevel] =
    Cogen[String].contramap(_.productPrefix)

  implicit val rcArb  = Arbitrary[ResourceConflict] {
    for {
      id <- arbitrary[Observation.Id]
    } yield ResourceConflict(id)
  }

  implicit val rcCogen: Cogen[ResourceConflict] =
    Cogen[Observation.Id].contramap(_.sid)

  implicit val inArb  = Arbitrary[InstrumentInUse] {
    for {
      id <- arbitrary[Observation.Id]
      i  <- arbitrary[Instrument]
    } yield InstrumentInUse(id, i)
  }

  implicit val inCogen: Cogen[InstrumentInUse] =
    Cogen[(Observation.Id, Instrument)].contramap(x => (x.sid, x.ins))

  implicit val notArb  = Arbitrary[Notification] {
    for {
      r <- arbitrary[ResourceConflict]
      a <- arbitrary[InstrumentInUse]
      s <- Gen.oneOf(r, a)
    } yield s
  }

  implicit val notCogen: Cogen[Notification] =
    Cogen[Either[ResourceConflict, InstrumentInUse]].contramap {
      case r: ResourceConflict => Left(r)
      case i: InstrumentInUse  => Right(i)
    }

  implicit val seqBatchCmdStateArb: Arbitrary[BatchCommandState] = Arbitrary(
    Gen.oneOf(BatchCommandState.Idle, BatchCommandState.Run, BatchCommandState.Stop)
  )

  implicit val seqBatchCmdStateCogen: Cogen[BatchCommandState] =
    Cogen[String].contramap(_.productPrefix)

  implicit val exQueueCmdStateCogen: Cogen[ExecutionQueue] =
    Cogen[(String, BatchCommandState, List[Observation.Id])]
      .contramap(x => (x.name, x.cmdState, x.queue))
}

object SeqexecModelArbitraries extends SeqexecModelArbitraries
