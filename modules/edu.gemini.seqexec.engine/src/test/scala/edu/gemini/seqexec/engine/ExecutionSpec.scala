package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._
import org.scalatest._
import scalaz.concurrent.Task

class ExecutionSpec extends FlatSpec with Matchers {

  val ok: Result = Result.OK(Result.Observed("dummyId"))
  val action: Action = Task(ok)
  val curr: Execution = Execution(List(ok.right, action.left))

  "currentify" should "be None only when an Execution is empty" in {
    assert(Execution.currentify(List(action, action)).nonEmpty)
    assert(Execution.currentify(Nil).isEmpty)
  }

  "uncurrentify" should "be None when not all actions are completed" in {
    assert(Execution(Nil).uncurrentify.isEmpty)
    assert(curr.uncurrentify.isEmpty)
  }

  "marking an index out of bounds" should "not modify the execution" in {
    assert(curr.mark(3)(Result.Error("")) === curr)
  }

  "marking an index inbound" should "modify the execution" in {
    assert(curr.mark(1)(Result.Error("")) !== curr)
  }

}
