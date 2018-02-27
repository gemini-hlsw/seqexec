// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.engine._
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.engine.Result.PauseContext
import edu.gemini.seqexec.model.{ActionType, UserDetails}
import edu.gemini.seqexec.model.Model.Instrument.F2
import edu.gemini.seqexec.model.Model.{ActionStatus, ImageQuality, WaterVapor, CloudCover, SkyBackground, Instrument, Operator, Resource, SequenceMetadata, SequenceState, Conditions}
import edu.gemini.seqexec.server.EngineMetadata.queuesL
import org.scalatest.{FlatSpec, Matchers}
import scalaz.concurrent.Task
import scalaz.stream.async
import scalaz.syntax.applicative._
import org.scalatest.Inside.inside
import scalaz.\/

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class SeqexecEngineSpec extends FlatSpec with Matchers {
  def configureTask(resource: Resource): Task[Result] = Task.delay(Result.OK(Result.Configured(resource)))
  def pendingAction(resource: Resource): Action =
    engine.fromTask(ActionType.Configure(resource), configureTask(resource))
  def running(resource: Resource): Action = pendingAction(resource).copy(state = Action.State(Action.Started, Nil))
  def done(resource: Resource): Action = pendingAction(resource).copy(state = Action.State(Action.Completed(Result.Configured(resource)), Nil))
  val fileId = "fileId"
  def observing: Action = engine.fromTask(ActionType.Observe, Task.delay(Result.OK(Result.Observed(fileId)))).copy(state = Action.State(Action.Started, Nil))
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

  private val seqexecEngine = SeqexecEngine(SeqexecEngine.defaultSettings)
  private def advanceOne(q: EventQueue, s0: executeEngine.StateType, put: Task[SeqexecFailure\/Unit]): Option[executeEngine.StateType] = {
    (put *> executeEngine.process(q.dequeue)(s0).take(1).runLast).unsafePerformSync.map(_._2)
  }
  private def advanceN(q: EventQueue, s0: executeEngine.StateType, put: Task[SeqexecFailure\/Unit], n: Int): Option[executeEngine.StateType] = {
    (put *> executeEngine.process(q.dequeue)(s0).take(n).runLast).unsafePerformSync.map(_._2)
  }
  private val seqId1 = "Dummy-1"
  private val seqObsId1 = new SPObservationID(seqId1)
  private val seqId2 = "Dummy-2"
  private val seqObsId2 = new SPObservationID(seqId2)
  private val seqId3 = "Dummy-3"
  private val seqObsId3 = new SPObservationID(seqId3)
  private def sequence(id: Sequence.Id): Sequence.State = Sequence.State.init(
    Sequence(
      id,
      SequenceMetadata(F2, None, ""),
      List(Step.init(1, None, Map(), Set(), List(List(pendingAction(F2)))))
    )
  )
  "SeqexecEngine addSequenceToQueue" should
    "add sequence id to queue" in {
    val s0 = Engine.State.empty(EngineMetadata.default)
      .copy(sequences = Map(seqId1 -> sequence(seqId1)))

    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId1))

    inside(sf.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId1)
    }
  }
  it should "not add sequence id if sequence does not exists" in {
    val badObsId = new SPObservationID("NonExistent-1")
    val s0 = Engine.State.empty(EngineMetadata.default)
      .copy(sequences = Map(seqId1 -> sequence(seqId1)))

    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, badObsId))

    inside(sf.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => assert(exq.isEmpty)
    }
  }
  it should "not add sequence id if sequence is running or completed" in {
    val s0 = Engine.State.empty(EngineMetadata.default)
      .copy(sequences = Map(seqId1 -> Sequence.State.status.set(SequenceState.Running.init)(sequence(seqId1)),
        seqId2 -> Sequence.State.status.set(SequenceState.Completed)(sequence(seqId2))
      ) )

    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceN(q, s0,
      seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId1) *>
      seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId2),
      2
    )

    inside(sf.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => assert(exq.isEmpty)
    }
  }
  it should "not add sequence id if already in queue" in {
    val s0 = Engine.State.empty(EngineMetadata.default)
      .copy(sequences = Map(seqId1 -> sequence(seqId1)))

    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceOne(q, s0, seqexecEngine.addSequenceToQueue(q, CalibrationQueueName, seqObsId1))

    inside(sf.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId1)
    }
  }

  "SeqexecEngine removeSequenceFromQueue" should
    "remove sequence id from queue" in {
    val s0 = Engine.State.empty(queuesL.set(Map(CalibrationQueueName -> List(seqId1, seqId2)))(EngineMetadata.default))
      .copy(sequences = Map(seqId1 -> sequence(seqId1),
        seqId2 -> sequence(seqId2)
      ) )

    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueName, seqObsId1))

    inside(sf.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId2)
    }
  }
  it should "not remove sequence id if sequence is running" in {
    val s0 = Engine.State.empty(queuesL.set(Map(CalibrationQueueName -> List(seqId1, seqId2)))(EngineMetadata.default))
      .copy(sequences = Map(seqId1 -> Sequence.State.status.set(SequenceState.Running.init)(sequence(seqId1)),
        seqId2 -> sequence(seqId2)
      ) )

    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceOne(q, s0, seqexecEngine.removeSequenceFromQueue(q, CalibrationQueueName, seqObsId1))

    inside(sf.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId1, seqId2)
    }
  }


  "SeqexecEngine moveSequenceInQueue" should
    "move sequence id inside queue" in {
    val s0 = Engine.State.empty(queuesL.set(Map(CalibrationQueueName -> List(seqId1, seqId2, seqId3)))(EngineMetadata.default))
      .copy(sequences = Map(seqId1 -> sequence(seqId1),
        seqId2 -> sequence(seqId2),
        seqId3 -> sequence(seqId3)
      ) )

    def testAdvance(obsId: SPObservationID, n: Int): Option[executeEngine.StateType] = {
      val q = async.boundedQueue[executeEngine.EventType](10)
      advanceOne(q, s0, seqexecEngine.moveSequenceInQueue(q, CalibrationQueueName, obsId, n))
    }

    val sf1 = testAdvance(seqObsId2, -1)

    inside(sf1.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId2, seqId1, seqId3)
    }

    val sf2 = testAdvance(seqObsId1, 2)

    inside(sf2.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId2, seqId3, seqId1)
    }

    val sf3 = testAdvance(seqObsId3, 4)

    inside(sf3.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId1, seqId2, seqId3)
    }

    val sf4 = testAdvance(seqObsId1, -2)

    inside(sf4.flatMap(x => queuesL.get(x.userData).get(CalibrationQueueName))) {
      case Some(exq) => exq shouldBe List(seqId1, seqId2, seqId3)
    }
  }

  "SeqexecEngine setOperator" should "set operator's name" in {
    val operator = Operator("Joe")
    val s0 = Engine.State.empty(EngineMetadata.default)
    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceN(q, s0, seqexecEngine.setOperator(q, UserDetails("", ""), operator), 2)

    inside(sf.flatMap((Engine.State.userDataL ^|-> EngineMetadata.operatorL).get(_))) {
      case Some(op) => op shouldBe operator
    }

  }

  "SeqexecEngine setImageQuality" should "set Image Quality condition" in {
    val iq = ImageQuality.Percent20
    val s0 = Engine.State.empty(EngineMetadata.default)
    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceN(q, s0, seqexecEngine.setImageQuality(q, iq, UserDetails("", "")), 2)

    inside(sf.map((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.iq).get(_))) {
      case Some(op) => op shouldBe iq
    }

  }

  "SeqexecEngine setWaterVapor" should "set Water Vapor condition" in {
    val wv = WaterVapor.Percent80
    val s0 = Engine.State.empty(EngineMetadata.default)
    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceN(q, s0, seqexecEngine.setWaterVapor(q, wv, UserDetails("", "")), 2)

    inside(sf.map((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.wv).get(_))) {
      case Some(op) => op shouldBe wv
    }

  }

  "SeqexecEngine setCloudCover" should "set Cloud Cover condition" in {
    val cc = CloudCover.Percent70
    val s0 = Engine.State.empty(EngineMetadata.default)
    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceN(q, s0, seqexecEngine.setCloudCover(q, cc, UserDetails("", "")), 2)

    inside(sf.map((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.cc).get(_))) {
      case Some(op) => op shouldBe cc
    }

  }

  "SeqexecEngine setSkyBackground" should "set Sky Background condition" in {
    val sb = SkyBackground.Percent50
    val s0 = Engine.State.empty(EngineMetadata.default)
    val q = async.boundedQueue[executeEngine.EventType](10)
    val sf = advanceN(q, s0, seqexecEngine.setSkyBackground(q, sb, UserDetails("", "")), 2)

    inside(sf.map((Engine.State.userDataL ^|-> EngineMetadata.conditionsL ^|-> Conditions.sb).get(_))) {
      case Some(op) => op shouldBe sb
    }

  }

}
