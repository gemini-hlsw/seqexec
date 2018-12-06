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
import org.scalatest.{FlatSpec, Matchers, NonImplicitAssertions}
import org.http4s.Uri._
import org.http4s.Uri

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import seqexec.engine
import seqexec.engine.Result.PauseContext
import seqexec.engine._
import seqexec.server.keywords.GDSClient
import seqexec.model.{ActionType, CalibrationQueueId, ClientId, Conditions, Observer, Operator, SequenceState, UserDetails}
import seqexec.model.enum._
import seqexec.model.enum.Resource.TCS
import monocle.Monocle._
import shapeless.tag

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements", "org.wartremover.warts.Throw"))
class SeqexecEngineSpec extends FlatSpec with Matchers with NonImplicitAssertions {
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
    ghostGdsControl = Simulated,
    gsaoiControl = Simulated,
    gwsControl = Simulated,
    nifsControl = Simulated,
    niriControl = Simulated,
    tcsControl = Simulated,
    odbNotifications = false,
    instForceError = false,
    failAt = 0,
    10.seconds,
    tag[GpiSettings][Giapi[IO]](Giapi.giapiConnectionIO(scala.concurrent.ExecutionContext.Implicits.global).connect.unsafeRunSync),
    tag[GhostSettings][Giapi[IO]](Giapi.giapiConnectionIO(scala.concurrent.ExecutionContext.Implicits.global).connect.unsafeRunSync),
    tag[GpiSettings][Uri](uri("http://localhost:8888/xmlrpc")),
    tag[GhostSettings][Uri](uri("http://localhost:8888/xmlrpc"))
  )

  def configureIO(resource: Resource): IO[Result] = IO.apply(Result.OK(Response.Configured(resource)))
  def pendingAction(resource: Resource): Action[IO] =
    engine.fromF[IO](ActionType.Configure(resource), configureIO(resource))
  def running(resource: Resource): Action[IO] = pendingAction(resource).copy(state = Action.State(
    Action.Started, Nil))
  def done(resource: Resource): Action[IO] = pendingAction(resource).copy(state = Action.State(
    Action.Completed(Response.Configured(resource)), Nil))
  val fileId = "fileId"
  def observing: Action[IO] = engine.fromF[IO](ActionType.Observe,
    IO.apply(Result.OK(Response.Observed(fileId)))).copy(state = Action.State(Action.Started, Nil))
  def fileIdReady: Action[IO] = observing.copy(state = Action.State(Action.Started,
    List(FileIdAllocated(fileId))))
  def observed: Action[IO] = observing.copy(state = Action.State(Action.Completed(Response.Observed(fileId)),
    List(FileIdAllocated(fileId))))
  def paused: Action[IO] = observing.copy(state = Action.State(Action.Paused(new PauseContext{}),
    List(FileIdAllocated(fileId))))

  "SeqexecEngine configStatus" should
    "build empty without tasks" in {
      SeqexecEngine.configStatus(Nil) shouldBe List.empty
    }
    it should "be all running if none has a result" in {
      val status = List(Resource.TCS -> ActionStatus.Running)
      val executions: List[List[Action[IO]]] = List(
        List(running(Resource.TCS)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be all running if none has a result 2" in {
      val status = List(Resource.TCS -> ActionStatus.Running, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action[IO]]] = List(
        List(running(Resource.TCS), running(Instrument.GmosN)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some running if none has a result even when the previous execution is complete" in {
      val status = List(Resource.TCS -> ActionStatus.Completed, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action[IO]]] = List(
        List(done(Resource.TCS)),
        List(done(Resource.TCS), running(Instrument.GmosN)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some pending if one will be done in the future" in {
      val status = List(Resource.TCS -> ActionStatus.Completed, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action[IO]]] = List(
        List(running(Instrument.GmosN)),
        List(done(Resource.TCS), done(Instrument.GmosN))
      )
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop at the first with running steps" in {
      val executions: List[List[Action[IO]]] = List(
        List(running(Instrument.GmosN)),
        List(running(Instrument.GmosN), running(Resource.TCS))
      )
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop evaluating where at least one is running even while some are done" in {
      val executions: List[List[Action[IO]]] = List(
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
      val executions: List[List[Action[IO]]] = List(
        List(pendingAction(Resource.TCS)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending with mixed" in {
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      val executions: List[List[Action[IO]]] = List(
        List(pendingAction(Resource.TCS), done(Instrument.GmosN)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending on mixed combinations" in {
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      val executions: List[List[Action[IO]]] = List(
        List(done(Resource.TCS)),
        List(done(Resource.TCS), pendingAction(Instrument.GmosN)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending with multiple resources" in {
      val executions: List[List[Action[IO]]] = List(
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
      val executions: List[List[Action[IO]]] = List(
        List(done(Resource.TCS), observing))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Running
    }
    it should "be done if there is a result observe" in {
      val executions: List[List[Action[IO]]] = List(
        List(done(Resource.TCS), observed))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Completed
    }
    it should "be running if there is a partial result with the file id" in {
      val executions: List[List[Action[IO]]] = List(
        List(done(Resource.TCS), fileIdReady))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Running
    }
    it should "be paused if there is a paused observe" in {
      val executions: List[List[Action[IO]]] = List(
        List(done(Resource.TCS), paused))
      SeqexecEngine.observeStatus(executions) shouldBe ActionStatus.Paused
    }

  private val sm = SeqexecMetrics.build[IO](Site.GS, new CollectorRegistry()).unsafeRunSync
  private val seqexecEngine = SeqexecEngine(GDSClient.alwaysOkClient, defaultSettings, sm)
  private def advanceOne(q: EventQueue, s0: EngineState, put: IO[Either[SeqexecFailure, Unit]]): IO[Option[EngineState]] =
    (put *> seqexecEngine.stream(q.dequeue)(s0).take(1).compile.last).map(_.map(_._2))

  private def advanceN(q: EventQueue, s0: EngineState, put: IO[Either[SeqexecFailure, Unit]], n: Long): IO[Option[EngineState]] =
    (put *> seqexecEngine.stream(q.dequeue)(s0).take(n).compile.last).map(_.map(_._2))

  private val seqId1 = "GS-2018B-Q-0-1"
  private val seqObsId1 = Observation.Id.unsafeFromString(seqId1)
  private val seqId2 = "GS-2018B-Q-0-2"
  private val seqObsId2 = Observation.Id.unsafeFromString(seqId2)
  private val seqId3 = "GS-2018B-Q-0-3"
  private val seqObsId3 = Observation.Id.unsafeFromString(seqId3)
  private val clientId = ClientId(UUID.randomUUID)
  private def sequence(id: Observation.Id): SequenceGen = SequenceGen(
    id,
    "",
    Instrument.F2,
    List(SequenceGen.PendingStepGen(1, Map(), Set.empty, SequenceGen.StepActionsGen(List(),
      Map(), _ => List(List(pendingAction(Instrument.F2)))
    )))
  )
  private def sequenceWithResources(id: Observation.Id, ins: Instrument, resources: Set[Resource]): SequenceGen = SequenceGen(
    id,
    "",
    ins,
    List(SequenceGen.PendingStepGen(1, Map(), resources,
      SequenceGen.StepActionsGen(List(), resources.map(r => r -> pendingAction(r)).toMap,
        _ =>List()
      )
    ))
  )

  "SeqexecEngine addSequenceToQueue" should
    "add sequence id to queue" in {
    val s0 = SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1)
      }
    }).unsafeRunSync
  }
  it should "not add sequence id if sequence does not exists" in {
    val badObsId = Observation.Id.unsafeFromString("NonExistent-1")
    val s0 = SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, badObsId))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => assert(exq.queue.isEmpty)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if sequence is running or completed" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init) >>>
      (EngineState.sequenceStateIndex(seqObsId2) ^|-> Sequence.State.status)
        .set(SequenceState.Completed)
      )(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceN(q, s0,
        seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1) *>
        seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId2),
        2)
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => assert(exq.queue.isEmpty)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if already in queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ :+ seqObsId1))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine addSequencesToQueue" should
    "add sequence ids to queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequence(seqObsId3)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequencesToQueue(q, CalibrationQueueId, List(seqObsId1, seqObsId2, seqObsId3)))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2, seqObsId3)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if sequence is running or completed" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequence(seqObsId3)) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status)
        .set(SequenceState.Running.init) >>>
      (EngineState.sequenceStateIndex(seqObsId2) ^|-> Sequence.State.status)
        .set(SequenceState.Completed)
      )(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0,
        seqexecEngine.addSequencesToQueue(q, CalibrationQueueId, List(seqObsId1, seqObsId2, seqObsId3)))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId3)
      }
    }).unsafeRunSync
  }

  it should "not add sequence id if already in queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ :+ seqObsId1))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.addSequencesToQueue(q, CalibrationQueueId, List(seqObsId1, seqObsId2)))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine clearQueue" should
    "remove all sequences from queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequence(seqObsId3)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ ++ List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.clearQueue(q, CalibrationQueueId))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List.empty
      }
    }).unsafeRunSync
  }

  "SeqexecEngine removeSequenceFromQueue" should
    "remove sequence id from queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ ++ List(seqObsId1, seqObsId2)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId2)
      }
    }).unsafeRunSync
  }

  it should "not remove sequence id if sequence is running" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ ++ List(seqObsId1, seqObsId2)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.cmdState).set(
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId)) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status).set(SequenceState
        .Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueId, seqObsId1))
    } yield {
      inside(sf.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
        case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine moveSequenceInQueue" should
    "move sequence id inside queue" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequence(seqObsId1)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequence(seqObsId2)) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequence(seqObsId3)) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).modify(_ ++ List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    def testAdvance(obsId: Observation.Id, n: Int): Option[EngineState] =
      (for {
        q <- async.boundedQueue[IO, executeEngine.EventType](10)
        r <- advanceOne(q, s0, seqexecEngine.moveSequenceInQueue(q, CalibrationQueueId, obsId, n, clientId))
      } yield r).unsafeRunSync

    val sf1 = testAdvance(seqObsId2, -1)

    inside(sf1.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId2, seqObsId1, seqObsId3)
    }

    val sf2 = testAdvance(seqObsId1, 2)

    inside(sf2.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId2, seqObsId3, seqObsId1)
    }

    val sf3 = testAdvance(seqObsId3, 4)

    inside(sf3.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2, seqObsId3)
    }

    val sf4 = testAdvance(seqObsId1, -2)

    inside(sf4.flatMap(x => EngineState.queues.get(x).get(CalibrationQueueId))) {
      case Some(exq) => exq.queue shouldBe List(seqObsId1, seqObsId2, seqObsId3)
    }
  }

  "SeqexecEngine nextRunnableObservations" should
    "return an empty set for an empty queue" in {
    val s0 = EngineState.default

    val r = SeqexecEngine.nextRunnableObservations(CalibrationQueueId)(s0)

    assert(r.isEmpty)
  }
  it should "return only the first observation that uses a common resource" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.GSAOI, Set(Instrument.GSAOI, TCS))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    val r = SeqexecEngine.nextRunnableObservations(CalibrationQueueId)(s0)

    r shouldBe Set(seqObsId1)
  }
  it should "return all observations with disjointed resource sets" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
      Instrument.F2, Set(Instrument.F2))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.GSAOI, Set(Instrument.GSAOI))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    val r = SeqexecEngine.nextRunnableObservations(CalibrationQueueId)(s0)

    r shouldBe Set(seqObsId1, seqObsId2, seqObsId3)
  }
  it should "not return observations with resources in use" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId2, seqObsId3)) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init))(EngineState.default)

    val r = SeqexecEngine.nextRunnableObservations(CalibrationQueueId)(s0)

    assert(r.isEmpty)
  }

  def testCompleted(oid: Observation.Id)(st: EngineState): Boolean = st.sequences.get(oid)
    .exists(_.seq.status.isCompleted)

  "SeqexecEngine startQueue" should "run sequences in queue" in {
    // seqObsId1 and seqObsId2 can be run immediately, but seqObsId3 must be run after seqObsId1
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.startQueue(q, CalibrationQueueId, Observer(""), UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(testCompleted(seqObsId1)(s) && testCompleted(seqObsId2)(s) && testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  it should "set observer for all sequences in queue" in {
    val observer = Observer("John Doe")
    // seqObsId1 and seqObsId2 can be run immediately, but seqObsId3 must be run after seqObsId1
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.startQueue(q, CalibrationQueueId, observer, UserDetails("", ""), clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf.map(_.sequences)) {
      case Some(s) => assert(
          s.get(seqObsId1).map(_.observer) === Some(Some(observer)) &&
          s.get(seqObsId2).map(_.observer) === Some(Some(observer)) &&
          s.get(seqObsId3).map(_.observer) === Some(Some(observer))
        )
    } ).unsafeRunSync
  }

  it should "load the sequences to the corresponding instruments" in {
    // seqObsId1 and seqObsId2 can be run immediately, but seqObsId3 must be run after seqObsId1
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      s1 <- advanceOne(q, s0,
        seqexecEngine.startQueue(q, CalibrationQueueId, Observer(""), UserDetails("", ""), clientId))
    } yield inside(s1.map(_.selected)) {
      case Some(sel1) => assert(sel1.get(Instrument.F2) === Some(seqObsId1) &&
                                sel1.get(Instrument.GmosS) === Some(seqObsId2))
    } ).unsafeRunSync
  }

  "SeqexecEngine stopQueue" should "stop running the sequences in the queue" in {
    // seqObsId1 and seqObsId2 will be run immediately, but seqObsId3 must be run after seqObsId1, and is the only one that will not run because of the stopQueue
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId) ^|-> ExecutionQueue.queue).set(
        List(seqObsId1, seqObsId2, seqObsId3)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      _ <- seqexecEngine.startQueue(q, CalibrationQueueId, Observer(""), UserDetails("", ""), clientId)
      _ <- seqexecEngine.stopQueue(q, CalibrationQueueId, clientId)
      sf <- seqexecEngine.stream(q.dequeue)(s0).map(_._2)
        .takeThrough(_.sequences.values.exists(_.seq.status.isRunning)).compile.last
    } yield inside(sf) {
      case Some(s) => assert(!testCompleted(seqObsId3)(s))
    } ).unsafeRunSync
  }

  "SeqexecEngine start sequence" should "not run sequence if running queue needs the same resources" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId3, sequenceWithResources(seqObsId3,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.queues ^|-? index(CalibrationQueueId)).modify(x => x.copy(cmdState =
        BatchCommandState.Run(Observer(""), UserDetails("", ""), clientId), queue = List(seqObsId1,
        seqObsId2))))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId3, UserDetails("", ""),clientId))
    } yield inside(sf.flatMap(_.sequences.get(seqObsId3))) {
      case Some(s) => assert(s.seq.status === SequenceState.Idle)
    } ).unsafeRunSync
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

  "SeqexecEngine" should "not run 2nd sequence because it's using the same resource" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId2, UserDetails("", ""), clientId))
    } yield {
      inside(sf.flatMap(EngineState.sequenceStateIndex(seqObsId2).getOption).map(_.status)) {
        case Some(status) => assert(status.isIdle)
      }
    }).unsafeRunSync

  }

  it should "run 2nd sequence when there are no shared resources" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.GmosS, Set(Instrument.GmosS))) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.start(q, seqObsId2, UserDetails("", ""), clientId))
    } yield {
      inside(sf.flatMap(EngineState.sequenceStateIndex(seqObsId2).getOption).map(_.status)) {
        case Some(status) => assert(status.isRunning)
      }
    }).unsafeRunSync
  }

  "SeqexecEngine configSystem" should "run a system configuration" in {
    val s0 = SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
      Instrument.F2, Set(Instrument.F2, TCS)))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId1, 1, TCS))
    } yield {
      inside(sf.flatMap((EngineState.sequences ^|-? index(seqObsId1)).getOption)) {
        case Some(s) => assertResult(Some(Action.Started))(
          s.seqGen.configActionCoord(1, TCS).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

  it should "not run a system configuration if sequence is running" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS))) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId1, 1, TCS))
    } yield {
      inside(sf.flatMap((EngineState.sequences ^|-? index(seqObsId1)).getOption)) {
        case Some(s) => assertResult(Some(Action.Idle))(
          s.seqGen.configActionCoord(1, TCS).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

  it should "not run a system configuration if system is in use" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.F2, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId2, 1, Instrument.F2))
    } yield {
      inside(sf.flatMap((EngineState.sequences ^|-? index(seqObsId2)).getOption)) {
        case Some(s) => assertResult(Some(Action.Idle))(
          s.seqGen.configActionCoord(1, Instrument.F2).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

  it should "run a system configuration when other sequence is running with other systems" in {
    val s0 = (SeqexecEngine.loadSequenceEndo(seqObsId1, sequenceWithResources(seqObsId1,
        Instrument.F2, Set(Instrument.GmosS, TCS))) >>>
      SeqexecEngine.loadSequenceEndo(seqObsId2, sequenceWithResources(seqObsId2,
        Instrument.F2, Set(Instrument.F2))) >>>
      (EngineState.sequenceStateIndex(seqObsId1) ^|-> Sequence.State.status).set(
        SequenceState.Running.init))(EngineState.default)

    (for {
      q <- async.boundedQueue[IO, executeEngine.EventType](10)
      sf <- advanceOne(q, s0, seqexecEngine.configSystem(q, seqObsId2, 1, Instrument.F2))
    } yield {
      inside(sf.flatMap((EngineState.sequences ^|-? index(seqObsId2)).getOption)) {
        case Some(s) => assertResult(Some(Action.Started))(
          s.seqGen.configActionCoord(1, Instrument.F2).map(s.seq.getSingleState)
        )
      }
    }).unsafeRunSync
  }

}
