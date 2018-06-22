// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.effect.IO
import org.scalatest._
import seqexec.model.ActionType

class ExecutionSpec extends FlatSpec with Matchers {

  private val observeResult: Result.Response = Result.Observed("dummyId")
  private val ok: Result = Result.OK(observeResult)
  private val completedAction: Action = fromIO(ActionType.Observe, IO(ok)).copy(state = Action.State(Action.Completed(observeResult), Nil))
  private val action: Action = fromIO(ActionType.Observe, IO(ok))
  private val curr: Execution = Execution(List(completedAction, action))

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
