// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.IO
import cats.implicits._
import fs2.async
import gem.Observation
import gem.enum.Site
import giapi.client.Giapi
import io.prometheus.client._
import java.time.LocalDate
import java.util.UUID
import org.scalatest.Inside.inside
import org.scalatest.{FlatSpec, Matchers}
import org.http4s.Uri._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import seqexec.engine
import seqexec.engine.Result.PauseContext
import seqexec.engine._
import seqexec.server.SeqexecEngine.Settings
import seqexec.server.keywords.GDSClient
import seqexec.model.{Conditions, Observer, Operator, SequenceState}
import seqexec.model.enum.{ActionStatus, CloudCover, ImageQuality, Instrument, Resource, SkyBackground, WaterVapor}
import seqexec.model.{ActionType, UserDetails}
import seqexec.model.enum.Resource.TCS
import monocle.Monocle._

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
@SuppressWarnings(Array("org.wartremover.warts.Throw"))
class SeqexecEngineSpec extends FlatSpec with Matchers {
  private val defaultSettings = Settings(Site.GS,
    odbHost = "localhost",
    date = LocalDate.of(2017, 1, 1),
    dhsURI = "http://localhost/",
    dhsControl = Simulated,
    f2Control = Simulated,
    gcalControl = Simulated,
    ghostControl = Simulated,
    gmosControl = Simulated,
    gnirsControl = Simulated,
    gpiControl = Simulated,
    gpiGdsControl = Simulated,
    gsaoiControl = Simulated,
    gwsControl = Simulated,
    nifsControl = Simulated,
    niriControl = Simulated,
    tcsControl = Simulated,
    odbNotifications = false,
    instForceError = false,
    failAt = 0,
    10.seconds,
    Giapi.giapiConnectionIO(scala.concurrent.ExecutionContext.Implicits.global).connect.unsafeRunSync,
    uri("http://localhost:8888/xmlrpc")
  )

  def configureIO(resource: Resource): IO[Result] = IO.apply(Result.OK(Result.Configured(resource)))
  def pendingAction(resource: Resource): Action =
    engine.fromIO(ActionType.Configure(resource), configureIO(resource))
  def running(resource: Resource): Action = pendingAction(resource).copy(state = Action.State(Action.Started, Nil))
  def done(resource: Resource): Action = pendingAction(resource).copy(state = Action.State(Action.Completed(Result.Configured(resource)), Nil))
  val fileId = "fileId"
  def observing: Action = engine.fromIO(ActionType.Observe, IO.apply(Result.OK(Result.Observed(fileId)))).copy(state = Action.State(Action.Started, Nil))
  def fileIdReady: Action = observing.copy(state = Action.State(Action.Started, List(Result.FileIdAllocated(fileId))))
  def observed: Action = observing.copy(state = Action.State(Action.Completed(Result.Observed(fileId)), List(Result.FileIdAllocated(fileId))))
  def paused: Action = observing.copy(state = Action.State(Action.Paused(new PauseContext{}), List(Result.FileIdAllocated(fileId))))

  "SeqexecEngine configStatus" should
    "build empty without tasks" in {
      SeqexecEngine.configStatus(Nil) shouldBe List.empty
    }
    it should "be all running if none has a result" in {
      val status = List(Resource.TCS -> ActionStatus.Running)
      val executions: List[List[Action]] = List(
        List(running(Resource.TCS)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be all running if none has a result 2" in {
      val status = List(Resource.TCS -> ActionStatus.Running, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action]] = List(
        List(running(Resource.TCS), running(Instrument.GmosN)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some running if none has a result even when the previous execution is complete" in {
      val status = List(Resource.TCS -> ActionStatus.Completed, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS)),
        List(done(Resource.TCS), running(Instrument.GmosN)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some pending if one will be done in the future" in {
      val status = List(Resource.TCS -> ActionStatus.Completed, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action]] = List(
        List(running(Instrument.GmosN)),
        List(done(Resource.TCS), done(Instrument.GmosN))
      )
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop at the first with running steps" in {
      val executions: List[List[Action]] = List(
        List(running(Instrument.GmosN)),
        List(running(Instrument.GmosN), running(Resource.TCS))
      )
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop evaluating where at least one is running even while some are done" in {
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS), done(Instrument.GmosN)),
        List(done(Resource.TCS), running(Instrument.GmosN)),
        List(pendingAction(Resource.TCS), pendingAction(Instrument.GmosN), pendingAction(Resource.Gcal)))
      val status = List(Resource.TCS -> ActionStatus.Completed, Resource.Gcal -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      SeqexecEngine.configStatus(executions) shouldBe status
    }

  "SeqexecEngine pending configStatus" should
    "build empty without tasks" in {
      SeqexecEngine.configStatus(Nil) shouldBe List.empty
    }
    it should "be all pending while one is running" in {
      val status = List(Resource.TCS -> ActionStatus.Pending)
      val executions: List[List[Action]] = List(
        List(pendingAction(Resource.TCS)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending with mixed" in {
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      val executions: List[List[Action]] = List(
        List(pendingAction(Resource.TCS), done(Instrument.GmosN)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending on mixed combinations" in {
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS)),
        List(done(Resource.TCS), pendingAction(Instrument.GmosN)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending with multiple resources" in {
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS), pendingAction(Instrument.GmosN)),
        List(done(Resource.TCS), pendingAction(Instrument.GmosN)),
        List(done(Resource.TCS), pendingAction(Instrument.GmosN), pendingAction(Resource.Gcal)))
      val status = List(Resource.TCS -> ActionStatus.Pending, Resource.Gcal -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }

  "SeqexecEngine observeStatus" should
    "be pending on empty" in {
      SeqexecEngine.observeStatus(Nil) shouldBe ActionStatus.Pending
    }
    it should "be running if there is an action observe" in {
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS), observing))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Running
    }
    it should "be done if there is a result observe" in {
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS), observed))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Completed
    }
    it should "be running if there is a partial result with the file id" in {
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS), fileIdReady))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Running
    }
    it should "be paused if there is a paused observe" in {
      val executions: List[List[Action]] = List(
        List(done(Resource.TCS), paused))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Paused
    }

  private val sm = SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry()).unsafeRunSync
  private val seqexecEngine = SeqexecEngine(GDSClient.alwaysOkClient, defaultSettings, sm)
  private def advanceOne(q: EventQueue, s0: EngineState, put: IO[Either[SeqexecFailure, Unit]]): IO[Option[EngineState]] =
    (put *> executeEngine.process(q.dequeue)(s0).take(1).compile.last).map(_.map(_._2))

  private def advanceN(q: EventQueue, s0: EngineState, put: IO[Either[SeqexecFailure, Unit]], n: Long): IO[Option[EngineState]] =
    (put *> executeEngine.process(q.dequeue)(s0).take(n).compile.last).map(_.map(_._2))

  private val seqId1 = "GS-2018B-Q-0-1"
  private val seqObsId1 = Observation.Id.unsafeFromString(seqId1)
  private val seqId2 = "GS-2018B-Q-0-2"
  private val seqObsId2 = Observation.Id.unsafeFromString(seqId2)
  private val seqId3 = "GS-2018B-Q-0-3"
  private val seqObsId3 = Observation.Id.unsafeFromString(seqId3)
  private def sequence(id: Observation.Id): SequenceGen = SequenceGen(
    id,
    "",
    Instrument.F2,
    List(SequenceGen.Step(1, Map(), Set.empty, _ => Step.init(1, None, List(List(pendingAction(Instrument.F2)))))))

  "SeqexecEngine addSequenceToQueue" should
    "add sequence id to queue" in {
    val s0 = SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
        case Some(exq) => exq shouldBe List(seqObsId1)
      }
    }).unsafeRunSync
  }
  it should "not add sequence id if sequence does not exists" in {
    val badObsId = Observation.Id.unsafeFromString("NonExistent-1")
    val s0 = SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, badObsId))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
        case Some(exq) => assert(exq.isEmpty)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if sequence is running or completed" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      (EngineState.executionState ^|-> Engine.State.sequences ^|-? index[Map[Observation.Id,Sequence.State], Observation.Id, Sequence.State](seqObsId1) ^|-> Sequence.State.status).set(SequenceState.Running.init) >>>
      (EngineState.executionState ^|-> Engine.State.sequences ^|-? index[Map[Observation.Id,Sequence.State], Observation.Id, Sequence.State](seqObsId2) ^|-> Sequence.State.status).set(SequenceState.Completed))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0,
        seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId1) *>
        seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId2),
        2)
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
        case Some(exq) => assert(exq.isEmpty)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if already in queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueName)).modify(_ :+ seqObsId1))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
        case Some(exq) => exq shouldBe List(seqObsId1)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine removeSequenceFromQueue" should
    "remove sequence id from queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueName)).modify(_ ++ List(seqObsId1, seqObsId2)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueName, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
        case Some(exq) => exq shouldBe List(seqObsId2)
      }
    }).unsafeRunSync
  }

  it should "not remove sequence id if sequence is running" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueName)).modify(_ ++ List(seqObsId1, seqObsId2)) >>>
      (EngineState.executionState ^|-> Engine.State.sequences ^|-? index(seqObsId1) ^|-> Sequence.State.status).set(SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueName, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
        case Some(exq) => exq shouldBe List(seqObsId1, seqObsId2)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine moveSequenceInQueue" should
    "move sequence id inside queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequence(seqObsId3)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueName)).modify(_ ++ List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    def testAdvance(obsId: Observation.Id, n: Int): Option[EngineState] =
      (for {
        q <- async.boundedQueue[IO, executeEngine.EventType](10)
        r <- advanceOne(q, s0, seqexecEngine.moveSequenceInQueue(q, CalibrationQueueName, obsId, n))
      } yield r).unsafeRunSync

    val sf1 = testAdvance(seqObsId2, -1)

    inside(sf1.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqObsId2, seqObsId1, seqObsId3)
    }

    val sf2 = testAdvance(seqObsId1, 2)

    inside(sf2.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqObsId2, seqObsId3, seqObsId1)
    }

    val sf3 = testAdvance(seqObsId3, 4)

    inside(sf3.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqObsId1, seqObsId2, seqObsId3)
    }

    val sf4 = testAdvance(seqObsId1, -2)

    inside(sf4.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqObsId1, seqObsId2, seqObsId3)
    }
  }

  "SeqexecEngine setOperator" should "set operator's name" in {
    val operator = Operator("Joe")
    val s0 = EngineState.default
    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setOperator(q, UserDetails("", ""), operator), 2)
    } yield {
      inside(sf.flatMap(EngineState.operator.get)) {
        case Some(op) => op shouldBe operator
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setImageQuality" should "set Image Quality condition" in {
    val iq = ImageQuality.Percent20
    val s0 = EngineState.default

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setImageQuality(q, iq, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.iq).get)) {
        case Some(op) => op shouldBe iq
      }
    }).unsafeRunSync

  }

  "SeqexecEngine setWaterVapor" should "set Water Vapor condition" in {
    val wv = WaterVapor.Percent80
    val s0 = EngineState.default
    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setWaterVapor(q, wv, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.wv).get(_))) {
        case Some(op) => op shouldBe wv
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setCloudCover" should "set Cloud Cover condition" in {
    val cc = CloudCover.Percent70
    val s0 = EngineState.default
    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setCloudCover(q, cc, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.cc).get(_))) {
        case Some(op) => op shouldBe cc
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setSkyBackground" should "set Sky Background condition" in {
    val sb = SkyBackground.Percent50
    val s0 = EngineState.default
    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setSkyBackground(q, sb, UserDetails("", "")), 2)
    } yield {
      inside(sf.map((EngineState.conditions ^|-> Conditions.sb).get(_))) {
        case Some(op) => op shouldBe sb
      }
    }).unsafeRunSync
  }

  "SeqexecEngine setObserver" should "set observer's name" in {
    val observer = Observer("Joe")
    val s0 = SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1))(EngineState.default)
    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0, seqexecEngine.setObserver(q, seqObsId1, UserDetails("", ""), observer), 2)
    } yield {
      inside(sf.flatMap((EngineState.sequences ^|-? index(seqObsId1)).getOption).flatMap(_.observer)) {
        case Some(op) => op shouldBe observer
      }
    }).unsafeRunSync
  }

  private def sequenceWithResources(id: Observation.Id, resources: Set[Resource]): SequenceGen = SequenceGen(
    id,
    "",
    Instrument.F2,
    List(SequenceGen.Step(1, Map(), resources, _ => Step.init(1, None, List(List(pendingAction(Instrument.F2)))))))

  "SeqexecEngine" should "not run 2nd sequence because it's using the same resource" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1, Set(Instrument.F2, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2, Set(Instrument.F2))) >>>
      (EngineState.executionState ^|-> Engine.State.sequences ^|-? index(seqObsId1) ^|-> Sequence.State.status).set(SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId2, UserDetails("", ""), UUID.randomUUID()))
    } yield {
      inside(sf.flatMap((EngineState.executionState ^|-> Engine.State.sequences ^|-? index(seqObsId2)).getOption).map(_.status)) {
        case Some(status) => assert(status.isIdle)
      }
    }).unsafeRunSync

  }

  it should "run 2nd sequence when there are no shared resources" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1, Set(Instrument.F2, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2, Set(Instrument.GmosS))) >>>
      (EngineState.executionState ^|-> Engine.State.sequences ^|-? index(seqObsId1) ^|-> Sequence.State.status).set(SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId2, UserDetails("", ""), UUID.randomUUID()))
    } yield {
      inside(sf.flatMap((EngineState.executionState ^|-> Engine.State.sequences ^|-? index(seqObsId2)).getOption).map(_.status)) {
        case Some(status) => assert(status.isRunning)
      }
    }).unsafeRunSync
  }

}
