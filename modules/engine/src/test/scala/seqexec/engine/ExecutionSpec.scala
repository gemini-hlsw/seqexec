// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.Id
import cats.data.NonEmptyList
import seqexec.model.ActionType

class ExecutionSpec extends munit.FunSuite {

  private object DummyResult extends Result.RetVal
  private val ok: Result[Nothing]         = Result.OK(DummyResult)
  private val completedAction: Action[Id] = fromF[Id](ActionType.Observe, ok)
    .copy(state = Action.State(Action.ActionState.Completed(DummyResult), Nil))
  private val action: Action[Id]          = fromF[Id](ActionType.Observe, ok)
  private val curr: Execution[Id]         = Execution(List(completedAction, action))

  test("currentify should be None only when an Execution is empty") {
    assert(Execution.currentify(NonEmptyList.of(action, action)).nonEmpty)
  }

  test("uncurrentify should be None when not all actions are completed") {
    assert(Execution(Nil).uncurrentify.isEmpty)
  }

  test("uncurrentify2 should be None when not all actions are completed") {
    assert(curr.uncurrentify.isEmpty)
  }

  test("marking an index out of bounds should not modify the execution") {
    assert(curr.mark(3)(Result.Error("")) == curr)
  }

  test("marking an index inbound should modify the execution") {
    assert(curr.mark(1)(Result.Error("")) != curr)
  }

}
