// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.implicits._
import fs2.concurrent.Queue
import org.scalatest.Inside.inside
import org.scalatest.NonImplicitAssertions
import org.scalatest.matchers.should.Matchers
import seqexec.server.TestCommon._
import seqexec.engine._
import seqexec.model.{Conditions, Observer, Operator, SequenceState, StepState, UserDetails}
import seqexec.model.enum._
import seqexec.model.enum.Resource.TCS
import monocle.Monocle._
import org.scalatest.flatspec.AnyFlatSpec

class SeqexecEngineSpec extends AnyFlatSpec with Matchers with NonImplicitAssertions {

  "SeqexecEngine setOperator" should "set operator's name" in {
    val operator = Operator("Joe")
    val s0 = EngineState.default[IO]
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setOperator(q, UserDetails("", ""), operator), 2)
    } yield {
      inside(sf.flatMap(EngineState.operator.get)) {
        case Some(op) => op shouldBe operator
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setImageQuality" should "set Image Quality condition" in {
    val iq = ImageQuality.Percent20
    val s0 = EngineState.default[IO]

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setImageQuality(q, iq, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.iq).get)) {
        case Some(op) => op shouldBe iq
      }
    }).unsafeRunSync

  }

  "SeqexecEngine setWaterVapor" should "set Water Vapor condition" in {
    val wv = WaterVapor.Percent80
    val s0 = EngineState.default[IO]
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setWaterVapor(q, wv, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.wv).get(_))) {
        case Some(op) => op shouldBe wv
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setCloudCover" should "set Cloud Cover condition" in {
    val cc = CloudCover.Percent70
    val s0 = EngineState.default[IO]
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setCloudCover(q, cc, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.cc).get(_))) {
        case Some(op) => op shouldBe cc
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setSkyBackground" should "set Sky Background condition" in {
    val sb = SkyBackground.Percent50
    val s0 = EngineState.default[IO]
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setSkyBackground(q, sb, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.sb).get(_))) {
        case Some(op) => op shouldBe sb
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setObserver" should "set observer's name" in {
    val observer = Observer("Joe")
    val s0 = ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine).apply(EngineState.default[IO])
    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setObserver(q, seqObsId1, UserDetails("", ""), observer), 2)
    } yield {
      inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId1)).getOption).flatMap(_.observer)) {
        case Some(op) => op shouldBe observer
      }
    }).unsafeRunSync
  }

  "SeqexecEngine" should "not run 2nd sequence because it's using the same resource" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.F2, Set(Instrument.F2)), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId2, UserDetails("", ""), clientId))
    } yield {
      inside(sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId2).getOption).map(_.status)) {
        case Some(status) => assert(status.isIdle)
      }
    }).unsafeRunSync

  }

  it should "run 2nd sequence when there are no shared resources" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS)), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.start(q, seqObsId2, UserDetails("", ""), clientId), 2)
    } yield {
      inside(sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId2).getOption).map(_.status)) {
        case Some(status) => assert(status.isRunning)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine configSystem" should "run a system configuration" in {
    val s0 = ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
      Instrument.F2, Set(Instrument.F2, TCS)), executeEngine).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId1, 1, TCS, clientId))
    } yield {
      inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId1)).getOption)) {
        case Some(s) => assertResult(Some(Action.ActionState.Started))(
          s.seqGen.configActionCoord(1, TCS).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

  it should "not run a system configuration if sequence is running" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS)), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId1, 1, TCS, clientId))
    } yield {
      inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId1)).getOption)) {
        case Some(s) => assertResult(Some(Action.ActionState.Idle))(
          s.seqGen.configActionCoord(1, TCS).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

  it should "not run a system configuration if system is in use" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.F2, Set(Instrument.F2)), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId2, 1, Instrument.F2, clientId))
    } yield {
      inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId2)).getOption)) {
        case Some(s) => assertResult(Some(Action.ActionState.Idle))(
          s.seqGen.configActionCoord(1, Instrument.F2).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

  it should "run a system configuration when other sequence is running with other systems" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.GmosS, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.F2, Set(Instrument.F2)), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId2, 1, Instrument.F2, clientId))
    } yield {
      inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId2)).getOption)) {
        case Some(s) => assertResult(Some(Action.ActionState.Started))(
          s.seqGen.configActionCoord(1, Instrument.F2).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

  "SeqexecEngine startFrom" should "start a sequence from an arbitrary step" in {
    val s0 = ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceNSteps(seqObsId1, 5), executeEngine).apply(EngineState.default[IO])
    val runStepId = 3

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      _  <- seqexecEngine.startFrom(q, seqObsId1, runStepId, clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning))
        .compile.last
    } yield {
      inside(sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.toSequence.steps)) {
        case Some(steps) => assertResult(Some(StepState.Skipped))(steps.get(0).map(_.status))
                            assertResult(Some(StepState.Skipped))(steps.get(1).map(_.status))
                            assertResult(Some(StepState.Completed))(steps.get(2).map(_.status))
      }
    }).unsafeRunSync
  }

  "SeqexecEngine startFrom" should "not start the sequence if there is a resource conflict" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, sequenceWithResources(seqObsId1,
      Instrument.F2, Set(Instrument.F2, TCS)), executeEngine) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.F2, Set(Instrument.F2)), executeEngine) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init)).apply(EngineState.default[IO])

    val runStepId = 2

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      _  <- seqexecEngine.startFrom(q, seqObsId2, runStepId, clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.get(seqObsId2).exists(_.seq.status.isRunning))
        .compile.last
    } yield {
      inside(sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId2).getOption).map(_.status)) {
        case Some(status) => assert(status.isIdle)
      }
    }).unsafeRunSync
  }

}
