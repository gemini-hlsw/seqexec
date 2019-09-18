// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Id
import cats.effect.IO
import cats.data.NonEmptyList
import org.scalatest.funsuite.AnyFunSuite
import seqexec.server.TestCommon._
import seqexec.engine._
import seqexec.model.enum._

class SeqexecEngineNSSpec extends AnyFunSuite {

  test("running after the first observe") {
    val executions: List[ParallelActions[IO]] = List(
      NonEmptyList.one(running(Resource.TCS)),
      NonEmptyList.one(observePartial))
    assert(StepsView.observeStatus(executions) === ActionStatus.Running)
  }
  test("running after the observe and configure") {
    val executions: List[ParallelActions[Id]] = List(
      NonEmptyList.one(running(Resource.TCS)),
      NonEmptyList.one(observePartial),
      NonEmptyList.one(done(Instrument.GmosN)))
    assert(StepsView.observeStatus(executions) === ActionStatus.Running)
  }
  test("running after the observe/configure/continue/complete") {
    val executions: List[ParallelActions[Id]] = List(
      NonEmptyList.one(running(Resource.TCS)),
      NonEmptyList.one(observePartial),
      NonEmptyList.one(done(Instrument.GmosN)),
      NonEmptyList.one(observed))
    assert(StepsView.observeStatus(executions) === ActionStatus.Running)
  }

}
