// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.Id
import org.scalatest._
import seqexec.model.ActionType

class ExecutionSpec extends FlatSpec with Matchers {

  private object DummyResult extends Result.RetVal
  private val ok: Result[Nothing] = Result.OK(DummyResult)
  private val completedAction: Action[Id] = fromF[Id](ActionType.Observe, ok).copy(state =
    Action.State(Action.ActionState.Completed(DummyResult), Nil))
  private val action: Action[Id] = fromF[Id](ActionType.Observe, ok)
  private val curr: Execution[Id] = Execution(List(completedAction, action))

  "currentify" should "be None only when an Execution is empty" in {
    assert(Execution.currentify(List(action, action)).nonEmpty)
  }

  "currentify2" should "be None only when an Execution is empty" in {
    assert(Execution.currentify(Nil).isEmpty)
  }

  "uncurrentify" should "be None when not all actions are completed" in {
    assert(Execution(Nil).uncurrentify.isEmpty)
  }

  "uncurrentify2" should "be None when not all actions are completed" in {
    assert(curr.uncurrentify.isEmpty)
  }

  "marking an index out of bounds" should "not modify the execution" in {
    assert(curr.mark(3)(Result.Error("")) === curr)
  }

  "marking an index inbound" should "modify the execution" in {
    assert(curr.mark(1)(Result.Error("")) !== curr)
  }

}
