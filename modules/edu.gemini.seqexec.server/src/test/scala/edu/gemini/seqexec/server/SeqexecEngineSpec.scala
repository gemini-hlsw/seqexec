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

  "SeqexecEngine" should
    "build empty configStatus" in {
      SeqexecEngine.configStatus(Nil) shouldBe Map.empty
    }
    it should "be all running if none has a result" in {
      val status = Map(Resource.TCS -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Resource.TCS))) // TCS is Running
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be all running if none has a result 2" in {
      val status = Map(Resource.TCS -> ActionStatus.Running, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Resource.TCS), running(Instrument.GmosN))) //TCS and GmosN Running
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some running if none has a result even when the previous execution is complete" in {
      val status = Map(Resource.TCS -> ActionStatus.Completed, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS)), // TCS Done
        List(done(Resource.TCS), running(Instrument.GmosN))) // Only GmosN Running
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "be some complete and some pending if one will be done in the future" in {
      val status = Map(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      val executions: List[List[Action \/ Result]] = List(
        List(running(Instrument.GmosN)), // Gmos Running
        List(done(Resource.TCS), done(Resource.TCS))) // TCS Running
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop at the first with running steps" in {
      val executions: List[List[Action \/ Result]] = List(
        List(running(Instrument.GmosN)),
        List(running(Instrument.GmosN), running(Resource.TCS)))
      val status = Map(Resource.TCS -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      SeqexecEngine.configStatus(executions) shouldBe status
    }
    it should "stop evaluating where at least one is running even while some are done" in {
      val executions: List[List[Action \/ Result]] = List(
        List(done(Resource.TCS), running(Instrument.GmosN)),
        List(done(Resource.TCS), running(Instrument.GmosN)),
        List(done(Resource.TCS), running(Instrument.GmosN), running(Resource.Gcal)))
      val status = Map(Resource.TCS -> ActionStatus.Completed, Resource.Gcal -> ActionStatus.Pending, Instrument.GmosN -> ActionStatus.Running)
      SeqexecEngine.configStatus(executions) shouldBe status
    }
}
