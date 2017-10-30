// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server

import edu.gemini.seqexec.engine.{Action, Result}
import edu.gemini.seqexec.engine
import edu.gemini.seqexec.model.ActionType
import edu.gemini.seqexec.model.Model.{ActionStatus, Instrument, Resource}
import org.scalatest.{FlatSpec, Matchers}

import scalaz.concurrent.Task
import scalaz.\/
import scalaz.syntax.either._

class SeqexecEngineSpec extends FlatSpec with Matchers {
  val dummyTask: Task[Result] = Task.delay(Result.OK(Result.Configured(Resource.TCS)))
  def running(resource: Resource): Action \/ Result =
    engine.fromTask(ActionType.Configure(resource), dummyTask).left[Result]
  def done(resource: Resource): Action \/ Result =
    Result.OK(Result.Configured(resource)).right[Action]
  def observing: Action \/ Result =
    engine.fromTask(ActionType.Observe, dummyTask).left[Result]
  def observed: Action \/ Result =
    Result.OK(Result.Observed("fileId")).right[Action]
  def fileId: Action \/ Result =
    Result.Partial(Result.FileIdAllocated("fileId"), engine.fromTask(ActionType.Observe, dummyTask)).right[Action]

  "SeqexecEngine configStatus" should
    "build empty without tasks" in {
      SeqexecEngine.configStatus(Nil) shouldBe List.empty
    }
    it should "be all running if none has a result" in {
      val status = List(Resource.TCS -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Resource.TCS)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be all running if none has a result 2" in {
      val status = List(Resource.TCS -> ActionStatus.Running, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Resource.TCS), running(Instrument.GmosN)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some running if none has a result even when the previous execution is complete" in {
      val status = List(Resource.TCS -> ActionStatus.Completed, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS)),
        List(done(Resource.TCS), running(Instrument.GmosN)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some pending if one will be done in the future" in {
      val status = List(Resource.TCS -> ActionStatus.Completed, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Instrument.GmosN)),
        List(done(Resource.TCS), done(Instrument.GmosN)))
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop at the first with running steps" in {
      val executions: List[List[Action \/ Result]] = List(
        List(running(Instrument.GmosN)),
        List(running(Instrument.GmosN), running(Resource.TCS)))
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop evaluating where at least one is running even while some are done" in {
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS), running(Instrument.GmosN)),
        List(done(Resource.TCS), running(Instrument.GmosN)),
        List(done(Resource.TCS), running(Instrument.GmosN), running(Resource.Gcal)))
      val status = List(Resource.TCS -> ActionStatus.Completed, Resource.Gcal -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      SeqexecEngine.configStatus(executions) shouldBe status
    }

  "SeqexecEngine pending configStatus" should
    "build empty without tasks" in {
      SeqexecEngine.pendingConfigStatus(Nil) shouldBe List.empty
    }
    it should "be all pending while one is running" in {
      val status = List(Resource.TCS -> ActionStatus.Pending)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Resource.TCS)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending with mixed" in {
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Resource.TCS), done(Instrument.GmosN)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending on mixed combinations" in {
      val status = List(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS)),
        List(done(Resource.TCS), running(Instrument.GmosN)))
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }
    it should "be all pending with multiple resources" in {
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS), running(Instrument.GmosN)),
        List(done(Resource.TCS), running(Instrument.GmosN)),
        List(done(Resource.TCS), running(Instrument.GmosN), running(Resource.Gcal)))
      val status = List(Resource.TCS -> ActionStatus.Pending, Resource.Gcal -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Pending)
      SeqexecEngine.pendingConfigStatus(executions) shouldBe status
    }

  "SeqexecEngine observeStatus" should
    "be pending on empty" in {
      SeqexecEngine.observeStatus(Nil, Nil) shouldBe ActionStatus.Pending
    }
    it should "be pending if anything is pending" in {
      val status = List(Resource.TCS -> ActionStatus.Pending)
      SeqexecEngine.observeStatus(Nil, status) shouldBe ActionStatus.Pending
    }
    it should "be pending if anything is running" in {
      val status = List(Resource.TCS -> ActionStatus.Pending)
      SeqexecEngine.observeStatus(Nil, status) shouldBe ActionStatus.Pending
    }
    it should "be running if there is an action observe" in {
      val status = List(Resource.TCS -> ActionStatus.Completed)
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS), observing))
      SeqexecEngine.observeStatus(executions, status) shouldBe ActionStatus.Running
    }
    it should "be done if there is a result observe" in {
      val status = List(Resource.TCS -> ActionStatus.Completed)
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS), observed))
      SeqexecEngine.observeStatus(executions, status) shouldBe ActionStatus.Completed
    }
    it should "be running if there is a partial result with the file id" in {
      val status = List(Resource.TCS -> ActionStatus.Completed)
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS), fileId))
      SeqexecEngine.observeStatus(executions, status) shouldBe ActionStatus.Running
    }
}
