// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Monoid
import cats.effect.IO
import cats.syntax.all._
import edu.gemini.spModel.config2.DefaultConfig
import edu.gemini.spModel.obsclass.ObsClass
import edu.gemini.spModel.obscomp.InstConstants.{
  ARC_OBSERVE_TYPE,
  DARK_OBSERVE_TYPE,
  OBSERVE_TYPE_PROP,
  OBS_CLASS_PROP,
  SCIENCE_OBSERVE_TYPE
}
import edu.gemini.spModel.seqcomp.SeqConfigNames.{ OBSERVE_KEY, OCS_KEY, TELESCOPE_KEY }
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.CLOUD_COVER_PROP
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.IMAGE_QUALITY_PROP
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.SKY_BACKGROUND_PROP
import edu.gemini.spModel.gemini.obscomp.SPSiteQuality.WATER_VAPOR_PROP
import fs2.concurrent.Queue
import lucuma.core.enum.Site
import io.prometheus.client.CollectorRegistry
import org.scalatest.Inside.inside
import org.scalatest.NonImplicitAssertions
import org.scalatest.matchers.should.Matchers
import seqexec.server.TestCommon._
import seqexec.engine._
import seqexec.model.{
  Conditions,
  Observer,
  Operator,
  SequenceState,
  StepId,
  StepState,
  SystemOverrides,
  UserDetails,
  UserPrompt
}
import seqexec.model.enum._
import seqexec.model.enum.Resource.TCS
import monocle.Monocle._
import org.scalatest.flatspec.AnyFlatSpec
import seqexec.engine.EventResult.{ Outcome, UserCommandResponse }
import seqexec.model.dhs.DataId
import seqexec.server.tcs.{
  DummyTargetKeywordsReader,
  DummyTcsKeywordsReader,
  TargetKeywordsReader
}
import seqexec.server.ConfigUtilOps._
import seqexec.server.SeqEvent.RequestConfirmation

class SeqexecEngineSpec extends AnyFlatSpec with Matchers with NonImplicitAssertions {

  "SeqexecEngine setOperator" should "set operator's name" in {
    val operator = Operator("Joe")
    val s0       = EngineState.default[IO]
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setOperator(q, UserDetails("", ""), operator), 2)
    } yield inside(sf.flatMap(EngineState.operator.get)) { case Some(op) =>
      op shouldBe operator
    }).unsafeRunSync()
  }

  "SeqexecEngine setImageQuality" should "set Image Quality condition" in {
    val iq = ImageQuality.Percent20
    val s0 = EngineState.default[IO]

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setImageQuality(q, iq, UserDetails("", "")), 2)
    } yield inside(sf.map((EngineState.conditions ^|-> Conditions.iq).get)) { case Some(op) =>
      op shouldBe iq
    }).unsafeRunSync()

  }

  "SeqexecEngine setWaterVapor" should "set Water Vapor condition" in {
    val wv = WaterVapor.Percent80
    val s0 = EngineState.default[IO]
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setWaterVapor(q, wv, UserDetails("", "")), 2)
    } yield inside(sf.map((EngineState.conditions ^|-> Conditions.wv).get(_))) { case Some(op) =>
      op shouldBe wv
    }).unsafeRunSync()
  }

  "SeqexecEngine setCloudCover" should "set Cloud Cover condition" in {
    val cc = CloudCover.Percent70
    val s0 = EngineState.default[IO]
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setCloudCover(q, cc, UserDetails("", "")), 2)
    } yield inside(sf.map((EngineState.conditions ^|-> Conditions.cc).get(_))) { case Some(op) =>
      op shouldBe cc
    }).unsafeRunSync()
  }

  "SeqexecEngine setSkyBackground" should "set Sky Background condition" in {
    val sb = SkyBackground.Percent50
    val s0 = EngineState.default[IO]
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setSkyBackground(q, sb, UserDetails("", "")), 2)
    } yield inside(sf.map((EngineState.conditions ^|-> Conditions.sb).get(_))) { case Some(op) =>
      op shouldBe sb
    }).unsafeRunSync()
  }

  "SeqexecEngine setObserver" should "set observer's name" in {
    val observer = Observer("Joe")
    val s0       = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, sequence(seqObsId1), executeEngine)
      .apply(EngineState.default[IO])
    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <-
        advanceN(q, s0, seqexecEngine.setObserver(q, seqObsId1, UserDetails("", ""), observer), 2)
    } yield inside(
      sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId1)).getOption).flatMap(_.observer)
    ) { case Some(op) =>
      op shouldBe observer
    }).unsafeRunSync()
  }

  "SeqexecEngine" should "not run 2nd sequence because it's using the same resource" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](
      seqObsId1,
      sequenceWithResources(seqObsId1, Instrument.F2, Set(Instrument.F2, TCS)),
      executeEngine
    ) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](
        seqObsId2,
        sequenceWithResources(seqObsId2, Instrument.F2, Set(Instrument.F2)),
        executeEngine
      ) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(
              q,
              s0,
              seqexecEngine.start(q,
                                  seqObsId2,
                                  UserDetails("", ""),
                                  Observer(""),
                                  clientId,
                                  RunOverride.Default
              )
            )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId2).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isIdle)
    }).unsafeRunSync()

  }

  it should "run 2nd sequence when there are no shared resources" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](
      seqObsId1,
      sequenceWithResources(seqObsId1, Instrument.F2, Set(Instrument.F2, TCS)),
      executeEngine
    ) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](
        seqObsId2,
        sequenceWithResources(seqObsId2, Instrument.GmosS, Set(Instrument.GmosS)),
        executeEngine
      ) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceN(
              q,
              s0,
              seqexecEngine.start(q,
                                  seqObsId2,
                                  UserDetails("", ""),
                                  Observer(""),
                                  clientId,
                                  RunOverride.Default
              ),
              2
            )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId2).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isRunning)
    }).unsafeRunSync()
  }

  "SeqexecEngine configSystem" should "run a system configuration" in {
    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](
        seqObsId1,
        sequenceWithResources(seqObsId1, Instrument.F2, Set(Instrument.F2, TCS)),
        executeEngine
      )
      .apply(EngineState.default[IO])

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <-
        advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId1, Observer(""), UserDetails("", ""), 1, TCS, clientId))
    } yield inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId1)).getOption)) {
      case Some(s) =>
        assertResult(Some(Action.ActionState.Idle))(
          s.seqGen.configActionCoord(1, TCS).map(s.seq.getSingleState)
        )
    }).unsafeRunSync()
  }

  it should "not run a system configuration if sequence is running" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](
      seqObsId1,
      sequenceWithResources(seqObsId1, Instrument.F2, Set(Instrument.F2, TCS)),
      executeEngine
    ) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <-
        advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId1, Observer(""), UserDetails("", ""), 1, TCS, clientId))
    } yield inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId1)).getOption)) {
      case Some(s) =>
        assertResult(Some(Action.ActionState.Idle))(
          s.seqGen.configActionCoord(1, TCS).map(s.seq.getSingleState)
        )
    }).unsafeRunSync()
  }

  it should "not run a system configuration if system is in use" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](
      seqObsId1,
      sequenceWithResources(seqObsId1, Instrument.F2, Set(Instrument.F2, TCS)),
      executeEngine
    ) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](
        seqObsId2,
        sequenceWithResources(seqObsId2, Instrument.F2, Set(Instrument.F2)),
        executeEngine
      ) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(
              q,
              s0,
              seqexecEngine.configSystem(q, seqObsId2, Observer(""), UserDetails("", ""), 1, Instrument.F2, clientId)
            )
    } yield inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId2)).getOption)) {
      case Some(s) =>
        assertResult(Some(Action.ActionState.Idle))(
          s.seqGen.configActionCoord(1, Instrument.F2).map(s.seq.getSingleState)
        )
    }).unsafeRunSync()
  }

  it should "run a system configuration when other sequence is running with other systems" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](
      seqObsId1,
      sequenceWithResources(seqObsId1, Instrument.F2, Set(Instrument.GmosS, TCS)),
      executeEngine
    ) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](
        seqObsId2,
        sequenceWithResources(seqObsId2, Instrument.F2, Set(Instrument.F2)),
        executeEngine
      ) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init)).apply(EngineState.default[IO])

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      sf <- advanceOne(
              q,
              s0,
              seqexecEngine.configSystem(q, seqObsId2, Observer(""), UserDetails("", ""), 1, Instrument.F2, clientId)
            )
    } yield inside(sf.flatMap((EngineState.sequences[IO] ^|-? index(seqObsId2)).getOption)) {
      case Some(s) =>
        assertResult(Some(Action.ActionState.Idle))(
          s.seqGen.configActionCoord(1, Instrument.F2).map(s.seq.getSingleState)
        )
    }).unsafeRunSync()
  }

  "SeqexecEngine startFrom" should "start a sequence from an arbitrary step" in {
    val s0        = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, sequenceNSteps(seqObsId1, 5), executeEngine)
      .apply(EngineState.default[IO])
    val runStepId = 3

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      _  <- seqexecEngine.startFrom(q,
                                    seqObsId1,
                                    Observer(""),
                                    runStepId,
                                    clientId,
                                    RunOverride.Default
            )
      sf <- seqexecEngine
              .stream(q.dequeue)(s0)
              .map(_._2)
              .takeThrough(_.sequences.values.exists(_.seq.status.isRunning))
              .compile
              .last
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.toSequence.steps)
    ) { case Some(steps) =>
      assertResult(Some(StepState.Skipped))(steps.get(0).map(_.status))
      assertResult(Some(StepState.Skipped))(steps.get(1).map(_.status))
      assertResult(Some(StepState.Completed))(steps.get(2).map(_.status))
    }).unsafeRunSync()
  }

  "SeqexecEngine startFrom" should "not start the sequence if there is a resource conflict" in {
    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](
      seqObsId1,
      sequenceWithResources(seqObsId1, Instrument.F2, Set(Instrument.F2, TCS)),
      executeEngine
    ) >>>
      ODBSequencesLoader.loadSequenceEndo[IO](
        seqObsId2,
        sequenceWithResources(seqObsId2, Instrument.F2, Set(Instrument.F2)),
        executeEngine
      ) >>>
      (EngineState.sequenceStateIndex[IO](seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init)).apply(EngineState.default[IO])

    val runStepId = 2

    (for {
      q  <- Queue.bounded[IO, executeEngine.EventType](10)
      _  <- seqexecEngine.startFrom(q,
                                    seqObsId2,
                                    Observer(""),
                                    runStepId,
                                    clientId,
                                    RunOverride.Default
            )
      sf <- seqexecEngine
              .stream(q.dequeue)(s0)
              .map(_._2)
              .takeThrough(_.sequences.get(seqObsId2).exists(_.seq.status.isRunning))
              .compile
              .last
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId2).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isIdle)
    }).unsafeRunSync()
  }

  private def testTargetSequence(
    targetName:  String,
    startStepId: StepId,
    obsClass:    List[ObsClass],
    obsType:     List[String]
  ): SequenceGen[IO] = {
    val resources = Set(Instrument.GmosS, TCS)

    SequenceGen[IO](
      id = seqObsId1,
      title = "",
      instrument = Instrument.GmosS,
      steps = obsClass.zip(obsType).zipWithIndex.map { case ((obC, obT), i) =>
        SequenceGen.PendingStepGen(
          startStepId + i,
          Monoid.empty[DataId],
          config = CleanConfig(
            new DefaultConfig(),
            Map(
              (TELESCOPE_KEY / "Base:name", targetName),
              (OBSERVE_KEY / OBS_CLASS_PROP, obC.headerValue()),
              (OBSERVE_KEY / OBSERVE_TYPE_PROP, obT)
            )
          ),
          resources = resources,
          _ => InstrumentSystem.Uncontrollable,
          generator = SequenceGen.StepActionsGen(
            configs = resources.map(r => r -> { _: SystemOverrides => pendingAction[IO](r) }).toMap,
            post = (_, _) => Nil
          )
        )
      }
    )
  }

  private def simpleSequenceWithTargetName(name: String): SequenceGen[IO] =
    testTargetSequence(name, 1, List(ObsClass.SCIENCE), List(SCIENCE_OBSERVE_TYPE))

  private def systemsWithTargetName(name: String): Systems[IO] =
    defaultSystems.copy(tcsKeywordReader =
      new DummyTcsKeywordsReader.DummyTcsKeywordReaderImpl[IO] {
        override def sourceATarget: TargetKeywordsReader[IO] =
          new DummyTargetKeywordsReader.DummyTargetKeywordsReaderImpl[IO] {
            override def objectName: IO[String] = name.pure[IO]
          }
      }
    )

  "SeqexecEngine start" should "start the sequence if it passes the target check" in {
    val systems = systemsWithTargetName("proof")

    val seq = simpleSequenceWithTargetName("proof")

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Default
                         )
                       )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isRunning)
    }).unsafeRunSync()
  }

  it should "not start the sequence if it fails the target check for science observations" in {
    val systems = systemsWithTargetName("other")

    val seq = testTargetSequence("proof", 1, List(ObsClass.SCIENCE), List(SCIENCE_OBSERVE_TYPE))

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Default
                         )
                       )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isIdle)
    }).unsafeRunSync()
  }

  it should "not start the sequence if it fails the target check for night calibrations" in {
    val systems = systemsWithTargetName("other")

    val seq = testTargetSequence("proof", 1, List(ObsClass.PROG_CAL), List(SCIENCE_OBSERVE_TYPE))

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Default
                         )
                       )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isIdle)
    }).unsafeRunSync()
  }

  it should "not start the sequence if it fails the target check for partner calibrations" in {
    val systems = systemsWithTargetName("other")

    val seq = testTargetSequence("proof", 1, List(ObsClass.PARTNER_CAL), List(SCIENCE_OBSERVE_TYPE))

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Default
                         )
                       )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isIdle)
    }).unsafeRunSync()
  }

  it should "pass the target check for ephemeris target" in {
    val systems = systemsWithTargetName("proof")

    val seq = testTargetSequence("proof.eph", 1, List(ObsClass.SCIENCE), List(SCIENCE_OBSERVE_TYPE))

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Default
                         )
                       )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isRunning)
    }).unsafeRunSync()
  }

  it should "start sequence that fails target check if forced" in {
    val systems = systemsWithTargetName("other")

    val seq = simpleSequenceWithTargetName("proof")

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Override
                         )
                       )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isRunning)
    }).unsafeRunSync()
  }

  it should "not check target for calibrations" in {
    val systems = systemsWithTargetName("other")

    val seq = simpleSequenceWithTargetName("proof")

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Override
                         )
                       )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isRunning)
    }).unsafeRunSync()
  }

  "SeqexecEngine startFrom" should "start the sequence if it passes the target check" in {
    val systems = systemsWithTargetName("proof")

    val seq = testTargetSequence("proof",
                                 1,
                                 List(ObsClass.ACQ, ObsClass.SCIENCE),
                                 List(ARC_OBSERVE_TYPE, SCIENCE_OBSERVE_TYPE)
    )

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <-
        advanceOne(
          q,
          s0,
          seqexecEngine.startFrom(q, seqObsId1, Observer(""), 2, clientId, RunOverride.Default)
        )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isRunning)
    }).unsafeRunSync()
  }

  it should "not start the sequence if it fails the target check" in {
    val systems = systemsWithTargetName("other")

    val seq = testTargetSequence("proof",
                                 1,
                                 List(ObsClass.ACQ, ObsClass.SCIENCE),
                                 List(ARC_OBSERVE_TYPE, SCIENCE_OBSERVE_TYPE)
    )

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      result        <-
        seqexecEngine.startFrom(q, seqObsId1, Observer(""), 2, clientId, RunOverride.Default) *>
          seqexecEngine.stream(q.dequeue)(s0).take(1).compile.last
    } yield inside(result) { case Some((out, sf)) =>
      inside(EngineState.sequenceStateIndex[IO](seqObsId1).getOption(sf).map(_.status)) {
        case Some(status) => assert(status.isIdle)
      }
      inside(out) {
        case UserCommandResponse(_,
                                 Outcome.Ok,
                                 Some(
                                   RequestConfirmation(
                                     UserPrompt.ChecksOverride(_, stpid, _),
                                     _
                                   )
                                 )
            ) =>
          assert(stpid === 2)
      }
    }).unsafeRunSync()
  }

  it should "start the sequence that fails target check if forced" in {
    val systems = systemsWithTargetName("other")

    val seq = testTargetSequence("proof",
                                 1,
                                 List(ObsClass.ACQ, ObsClass.SCIENCE),
                                 List(ARC_OBSERVE_TYPE, SCIENCE_OBSERVE_TYPE)
    )

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <-
        advanceOne(
          q,
          s0,
          seqexecEngine.startFrom(q, seqObsId1, Observer(""), 2, clientId, RunOverride.Override)
        )
    } yield inside(sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption)) {
      case Some(s) =>
        assert(s.status.isRunning)
        inside(s.currentStep) { case Some(t) =>
          assert(t.id === 2)
        }
    }).unsafeRunSync()
  }

  "SeqexecEngine startFrom" should "not check target for calibrations" in {
    val systems = systemsWithTargetName("other")

    val seq = testTargetSequence("proof",
                                 1,
                                 List(ObsClass.DAY_CAL, ObsClass.DAY_CAL),
                                 List(ARC_OBSERVE_TYPE, DARK_OBSERVE_TYPE)
    )

    val s0 = ODBSequencesLoader
      .loadSequenceEndo[IO](seqObsId1, seq, executeEngine)
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, systems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <-
        advanceOne(
          q,
          s0,
          seqexecEngine.startFrom(q, seqObsId1, Observer(""), 2, clientId, RunOverride.Default)
        )
    } yield inside(
      sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption).map(_.status)
    ) { case Some(status) =>
      assert(status.isRunning)
    }).unsafeRunSync()
  }

  private val testConditionsSequence: SequenceGen[IO] = {
    val resources         = Set(Instrument.GmosS, TCS)
    val obsClass          = List(ObsClass.PROG_CAL, ObsClass.SCIENCE)
    val obsType           = List(DARK_OBSERVE_TYPE, SCIENCE_OBSERVE_TYPE)
    val startStepId       = 1;
    val ObsConditionsProp = "obsConditions"
    val reqConditions     = Map(
      OCS_KEY / ObsConditionsProp / WATER_VAPOR_PROP    -> "20",
      OCS_KEY / ObsConditionsProp / SKY_BACKGROUND_PROP -> "20",
      OCS_KEY / ObsConditionsProp / IMAGE_QUALITY_PROP  -> "20",
      OCS_KEY / ObsConditionsProp / CLOUD_COVER_PROP    -> "50"
    )

    SequenceGen[IO](
      id = seqObsId1,
      title = "",
      instrument = Instrument.GmosS,
      steps = obsClass.zip(obsType).zipWithIndex.map { case ((obC, obT), i) =>
        SequenceGen.PendingStepGen(
          startStepId + i,
          Monoid.empty[DataId],
          config = CleanConfig(
            new DefaultConfig(),
            Map(
              (OBSERVE_KEY / OBS_CLASS_PROP, obC.headerValue()),
              (OBSERVE_KEY / OBSERVE_TYPE_PROP, obT)
            ) ++ reqConditions
          ),
          resources = resources,
          _ => InstrumentSystem.Uncontrollable,
          generator = SequenceGen.StepActionsGen(
            configs = resources.map(r => r -> { _: SystemOverrides => pendingAction[IO](r) }).toMap,
            post = (_, _) => Nil
          )
        )
      }
    )
  }

  "SeqexecEngine start" should "start the sequence if it passes the conditions check" in {

    val seq = testConditionsSequence

    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, seq, executeEngine) >>>
      (EngineState.conditions[IO] ^|-> Conditions.iq).set(ImageQuality.Percent20) >>>
      (EngineState.conditions[IO] ^|-> Conditions.wv).set(WaterVapor.Percent20) >>>
      (EngineState.conditions[IO] ^|-> Conditions.sb).set(SkyBackground.Percent20) >>>
      (EngineState.conditions[IO] ^|-> Conditions.cc).set(CloudCover.Percent50))
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, defaultSystems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <- advanceOne(
                         q,
                         s0,
                         seqexecEngine.start(q,
                                             seqObsId1,
                                             UserDetails("", ""),
                                             Observer(""),
                                             clientId,
                                             RunOverride.Default
                         )
                       )
    } yield inside(sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption)) {
      case Some(s) =>
        assert(s.status.isRunning)
        inside(s.currentStep) { case Some(t) =>
          assert(t.id === 1)
        }
    }).unsafeRunSync()
  }

  it should "not start the sequence if it fails the conditions check" in {
    val seq = testConditionsSequence

    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, seq, executeEngine) >>>
      (EngineState.conditions[IO] ^|-> Conditions.iq).set(ImageQuality.Percent70) >>>
      (EngineState.conditions[IO] ^|-> Conditions.wv).set(WaterVapor.Percent20) >>>
      (EngineState.conditions[IO] ^|-> Conditions.sb).set(SkyBackground.Percent20) >>>
      (EngineState.conditions[IO] ^|-> Conditions.cc).set(CloudCover.Percent50))
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, defaultSystems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      result        <-
        seqexecEngine.start(q,
                            seqObsId1,
                            UserDetails("", ""),
                            Observer(""),
                            clientId,
                            RunOverride.Default
        ) *>
          seqexecEngine.stream(q.dequeue)(s0).take(1).compile.last
    } yield inside(result) { case Some((out, sf)) =>
      inside(EngineState.sequenceStateIndex[IO](seqObsId1).getOption(sf).map(_.status)) {
        case Some(status) => assert(status.isIdle)
      }
      inside(out) {
        case UserCommandResponse(_,
                                 Outcome.Ok,
                                 Some(
                                   RequestConfirmation(
                                     UserPrompt.ChecksOverride(_, stpid, _),
                                     _
                                   )
                                 )
            ) =>
          assert(stpid === 1)
      }
    }).unsafeRunSync()
  }

  it should "start the sequence that fails conditions check if forced" in {

    val seq = testConditionsSequence

    val s0 = (ODBSequencesLoader.loadSequenceEndo[IO](seqObsId1, seq, executeEngine) >>>
      (EngineState.conditions[IO] ^|-> Conditions.iq).set(ImageQuality.Percent70) >>>
      (EngineState.conditions[IO] ^|-> Conditions.wv).set(WaterVapor.Percent20) >>>
      (EngineState.conditions[IO] ^|-> Conditions.sb).set(SkyBackground.Percent20) >>>
      (EngineState.conditions[IO] ^|-> Conditions.cc).set(CloudCover.Percent50))
      .apply(EngineState.default[IO])

    (for {
      sm            <- SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry())
      seqexecEngine <- SeqexecEngine.build(Site.GS, defaultSystems, defaultSettings, sm)
      q             <- Queue.bounded[IO, executeEngine.EventType](10)
      sf            <-
        advanceN(
          q,
          s0,
          seqexecEngine
            .start(q, seqObsId1, UserDetails("", ""), Observer(""), clientId, RunOverride.Override),
          3
        )
    } yield inside(sf.flatMap(EngineState.sequenceStateIndex[IO](seqObsId1).getOption)) {
      case Some(s) =>
        assert(s.status.isRunning)
        inside(s.currentStep) { case Some(t) =>
          assert(t.id === 1)
        }
    }).unsafeRunSync()
  }

}
