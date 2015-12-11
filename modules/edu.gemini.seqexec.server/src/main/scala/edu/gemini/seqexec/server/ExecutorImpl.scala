package edu.gemini.seqexec.server

import java.util.concurrent.{ScheduledExecutorService, ScheduledThreadPoolExecutor}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scalaz._
import Scalaz._

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.Executor._
import edu.gemini.seqexec.server.SeqexecFailure._
import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.obscomp.InstConstants.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz.concurrent.Task

/** An executor that maintains state in a pair of `TaskRef`s. */
class ExecutorImpl private (cancelRef: TaskRef[Set[SPObservationID]], stateRef: TaskRef[Map[SPObservationID, Executor.ExecState]]) {

  private def recordState(id: SPObservationID)(s: ExecState): Task[Unit] =
    stateRef.modify(_ + (id -> s))

  private def go(id: SPObservationID): Task[Boolean] = 
    cancelRef.get.map(!_(id))

  private def initialExecState(sequenceConfig: ConfigSequence): ExecState =
    ExecState.initial(sequenceConfig.getAllSteps.toList.map(Step.step))

  // todo: it is an error to run a sequence with an existing ExecState != s
  private def runSeq(id: SPObservationID, s: ExecState): Task[(ExecState, NonEmptyList[SeqexecFailure] \/ Unit)] =
    cancelRef.modify(_ - id) *> recordState(id)(s) *> run(go(id), recordState(id))(s)

  def start(id: SPObservationID, sequenceConfig: ConfigSequence): Task[(ExecState, NonEmptyList[SeqexecFailure] \/ Unit)] =
    runSeq(id, initialExecState(sequenceConfig))

  def continue(id: SPObservationID): Task[(Option[ExecState], NonEmptyList[SeqexecFailure] \/ Unit)] =
    getState(id) >>= {
      case -\/(e) => Task.now((None, NonEmptyList(e).left))
      case \/-(s) => runSeq(id, s).map(_.leftMap(_.some))
    }

  def getState(id: SPObservationID): Task[SeqexecFailure \/ ExecState] =
    stateRef.get.map(_.get(id) \/> InvalidOp("Sequence has not started."))

  def stop(id: SPObservationID): Task[SeqexecFailure \/ Unit] =
    getState(id) >>= {
      case \/-(s) => cancelRef.modify(s => s + id).as(\/-(()))
      case -\/(e) => Task.now(-\/(e))
    }

  def stateDescription(state: ExecState): String = {
    "Completed " + state.completed.length + " steps out of " + (state.completed.length + state.remaining.length) +
      ( state.completed.zipWithIndex.map(a => (a._1, a._2+1)) map {
        case (Ok(StepResult(_,ObserveResult(label))), idx) => s"Step $idx completed with label $label"
        case (Failed(result), idx) => s"Step $idx failed with error " + result.map(SeqexecFailure.explain).toList.mkString("\n", "\n", "")
        case (Skipped, idx)        => s"Step $idx skipped"
      }
    ).mkString("\n", "\n", "")
  }

}

object ExecutorImpl {
 
  def newInstance: Task[ExecutorImpl] =
    for {
      cancel <- TaskRef.newTaskRef[Set[SPObservationID]](Set.empty)
      state  <- TaskRef.newTaskRef[Map[SPObservationID, Executor.ExecState]](Map.empty)
    } yield new ExecutorImpl(cancel, state)

}
