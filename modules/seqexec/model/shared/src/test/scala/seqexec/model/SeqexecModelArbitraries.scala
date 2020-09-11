// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.syntax.all._
import java.util.UUID
import gem.Observation
import gem.arb.ArbEnumerated.{arbEnumerated => oldArbEnumerated}
import gem.arb.ArbEnumerated.{cogEnumerated => oldCogEnumerated}
import lucuma.core.util.arb.ArbEnumerated._
import gem.arb.ArbObservation._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Gen
import org.scalacheck.Arbitrary._
import scala.collection.immutable.SortedMap
import squants.time._
import seqexec.model.enum._
import seqexec.model.events.SingleActionEvent
import seqexec.model.arb.ArbStep._
import seqexec.model.arb.ArbClientId._

trait SeqexecModelArbitraries {

  private val maxListSize = 2

  implicit val opArb = Arbitrary[Operator] { Gen.alphaStr.map(Operator.apply) }

  implicit val conArb = Arbitrary[Conditions] {
    for {
      cc <- arbitrary[CloudCover]
      iq <- arbitrary[ImageQuality]
      sb <- arbitrary[SkyBackground]
      wv <- arbitrary[WaterVapor]
    } yield Conditions(cc, iq, sb, wv)
  }

  // N.B. We don't want to auto derive this to limit the size of the lists for performance reasons
  implicit def sequencesQueueArb[A](
    implicit arb: Arbitrary[A]): Arbitrary[SequencesQueue[A]] = Arbitrary {
    for {
      b <- Gen.listOfN[A](maxListSize, arb.arbitrary)
      c <- arbitrary[Conditions]
      o <- arbitrary[Option[Operator]]
      // We are already testing serialization of conditions and Strings
      // Let's reduce the test space by only testing the list of items
    } yield SequencesQueue(Map.empty, c, o, SortedMap.empty, b)
  }

  implicit val queueIdArb: Arbitrary[QueueId] = Arbitrary {
    arbitrary[UUID].map(QueueId)
  }

  implicit val qidCogen: Cogen[QueueId] =
    Cogen[UUID].contramap(_.self)

  implicit val actArb = Arbitrary[ActionType] {
    for {
      c <- arbitrary[Resource].map(ActionType.Configure.apply)
      a <- Gen.oneOf(ActionType.Observe, ActionType.Undefined)
      b <- Gen.oneOf(c, a)
    } yield b
  }

  implicit val udArb = Arbitrary[UserDetails] {
    for {
      u <- arbitrary[String]
      n <- arbitrary[String]
    } yield UserDetails(u, n)
  }

  implicit val obArb = Arbitrary[Observer] { Gen.alphaStr.map(Observer.apply) }
  implicit val smArb = Arbitrary[SequenceMetadata] {
    for {
      i <- arbitrary[Instrument]
      o <- arbitrary[Option[Observer]]
      n <- Gen.alphaStr
    } yield SequenceMetadata(i, o, n)
  }

  implicit val sqrArb = Arbitrary[SequenceState.Running] {
    for {
      u <- arbitrary[Boolean]
      i <- arbitrary[Boolean]
    } yield SequenceState.Running(u, i)
  }

  implicit val sqsArb = Arbitrary[SequenceState] {
    for {
      f <- Gen.oneOf(SequenceState.Completed,
                     SequenceState.Idle)
      r <- arbitrary[SequenceState.Running]
      a <- arbitrary[String].map(SequenceState.Failed.apply)
      s <- Gen.oneOf(f, r, a)
    } yield s
  }

  implicit val svArb = Arbitrary[SequenceView] {
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

  implicit val opCogen: Cogen[Operator] =
    Cogen[String].contramap(_.value)

  implicit val obCogen: Cogen[Observer] =
    Cogen[String].contramap(_.value)

  implicit val sqsCogen: Cogen[SequenceState] =
    Cogen[String].contramap(_.productPrefix)

  implicit val udCogen: Cogen[UserDetails] =
    Cogen[(String, String)].contramap(u => (u.username, u.displayName))

  implicit val smCogen: Cogen[SequenceMetadata] =
    Cogen[(Instrument, Option[Observer], String)].contramap(s =>
      (s.instrument, s.observer, s.name))

  implicit val svCogen: Cogen[SequenceView] =
    Cogen[(Observation.Id,
           SequenceMetadata,
           SequenceState,
           List[Step],
           Option[Int])].contramap(s =>
      (s.id, s.metadata, s.status, s.steps, s.willStopIn))

  implicit def sqCogen[A: Cogen]: Cogen[SequencesQueue[A]] =
    Cogen[(Conditions, Option[Operator], List[A])].contramap(s =>
      (s.conditions, s.operator, s.sessionQueue))

  implicit val conCogen: Cogen[Conditions] =
    Cogen[(CloudCover, ImageQuality, SkyBackground, WaterVapor)].contramap(c =>
      (c.cc, c.iq, c.sb, c.wv))

  implicit val seqBatchCmdRunArb: Arbitrary[BatchCommandState.Run] = Arbitrary {
    for {
      observer <- arbitrary[Observer]
      user     <- arbitrary[UserDetails]
      clid     <- arbitrary[ClientId]
    } yield BatchCommandState.Run(observer, user, clid)
  }

  implicit val seqBatchCmdStateArb: Arbitrary[BatchCommandState] = Arbitrary(
    Gen.frequency(
      (2, Gen.oneOf(BatchCommandState.Idle, BatchCommandState.Stop)),
      (1, arbitrary[BatchCommandState.Run]))
  )

  implicit val seqBatchCmdStateCogen: Cogen[BatchCommandState] =
    Cogen[(String, Option[Observer], Option[UserDetails], Option[ClientId])]
      .contramap {
        case r @ BatchCommandState.Run(obs, usd, cid) =>
          (r.productPrefix, obs.some, usd.some, cid.some)
        case o => (o.productPrefix, None, None, None)
      }

  implicit val executionQueueViewArb: Arbitrary[ExecutionQueueView] =
    Arbitrary {
      for {
        id <- arbitrary[QueueId]
        n  <- arbitrary[String]
        s  <- arbitrary[BatchCommandState]
        xs <- arbitrary[BatchExecState]
        q  <- arbitrary[List[Observation.Id]]
      } yield ExecutionQueueView(id, n, s, xs, q)
    }

  implicit val executionQueueViewCogen: Cogen[ExecutionQueueView] =
    Cogen[(QueueId,
           String,
           BatchCommandState,
           BatchExecState,
           List[Observation.Id])]
      .contramap(x => (x.id, x.name, x.cmdState, x.execState, x.queue))

  implicit val arbUserLoginRequest: Arbitrary[UserLoginRequest] =
    Arbitrary {
      for {
        u <- arbitrary[String]
        p <- arbitrary[String]
      } yield UserLoginRequest(u, p)
    }

  implicit val userLoginRequestCogen: Cogen[UserLoginRequest] =
    Cogen[(String, String)].contramap(x => (x.username, x.password))

  implicit val arbTimeUnit: Arbitrary[TimeUnit] =
    Arbitrary {
      Gen.oneOf(Nanoseconds,
                Microseconds,
                Milliseconds,
                Seconds,
                Minutes,
                Hours,
                Days)
    }

  implicit val timeUnitCogen: Cogen[TimeUnit] =
    Cogen[String]
      .contramap(_.symbol)

  implicit val saoStartArb: Arbitrary[SingleActionOp.Started] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        s <- arbitrary[StepId]
        r <- arbitrary[Resource]
      } yield SingleActionOp.Started(o, s, r)
    }

  implicit val saoStartCogen: Cogen[SingleActionOp.Started] =
    Cogen[(Observation.Id, StepId, Resource)]
      .contramap(x => (x.sid, x.stepId, x.resource))

  implicit val saoCompleteArb: Arbitrary[SingleActionOp.Completed] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        s <- arbitrary[StepId]
        r <- arbitrary[Resource]
      } yield SingleActionOp.Completed(o, s, r)
    }

  implicit val saoCompleteCogen: Cogen[SingleActionOp.Completed] =
    Cogen[(Observation.Id, StepId, Resource)]
      .contramap(x => (x.sid, x.stepId, x.resource))

  implicit val saoErrorArb: Arbitrary[SingleActionOp.Error] =
    Arbitrary {
      for {
        o <- arbitrary[Observation.Id]
        s <- arbitrary[StepId]
        r <- arbitrary[Resource]
        m <- arbitrary[String]
      } yield SingleActionOp.Error(o, s, r, m)
    }

  implicit val saoErrorCogen: Cogen[SingleActionOp.Error] =
    Cogen[(Observation.Id, StepId, Resource, String)]
      .contramap(x => (x.sid, x.stepId, x.resource, x.msg))

  implicit val saoArb = Arbitrary[SingleActionOp] {
    for {
      s <- arbitrary[SingleActionOp.Started]
      c <- arbitrary[SingleActionOp.Completed]
      e <- arbitrary[SingleActionOp.Error]
      m <- Gen.oneOf(s, c, e)
    } yield m
  }

  implicit val saoCogen: Cogen[SingleActionOp] =
    Cogen[Either[SingleActionOp.Started,
                 Either[SingleActionOp.Completed, SingleActionOp.Error]]]
      .contramap {
        case s: SingleActionOp.Started   => Left(s)
        case c: SingleActionOp.Completed => Right(Left(c))
        case e: SingleActionOp.Error     => Right(Right(e))
      }

  implicit val arbSingleActionEvent: Arbitrary[SingleActionEvent] =
    Arbitrary {
      for {
        e <- arbitrary[SingleActionOp]
      } yield SingleActionEvent(e)
    }

  implicit val singleActionEventCogen: Cogen[SingleActionEvent] =
    Cogen[SingleActionOp]
      .contramap(_.op)
}

object SeqexecModelArbitraries extends SeqexecModelArbitraries
