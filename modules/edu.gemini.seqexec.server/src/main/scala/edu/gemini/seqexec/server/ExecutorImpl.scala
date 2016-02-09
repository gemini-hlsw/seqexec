package edu.gemini.seqexec.server

import edu.gemini.seqexec.shared.{SeqFailure, SeqExecService}
import edu.gemini.spModel.core.Peer

import scalaz._
import Scalaz._

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.Executor._
import edu.gemini.seqexec.server.SeqexecFailure._
import edu.gemini.spModel.config2.ConfigSequence

import scalaz.concurrent.Task

/** An executor that maintains state in a pair of `TaskRef`s. */
class ExecutorImpl private (cancelRef: TaskRef[Set[SPObservationID]], stateRef: TaskRef[Map[SPObservationID, Executor.ExecState]]) {

  private var loc = new Peer("localhost", 8443, null)

  def host(): Peer = loc
  def host(l: Peer): Unit = { loc = l }

  private def recordState(id: SPObservationID)(s: ExecState): Task[Unit] =
    stateRef.modify(_ + (id -> s))

  private def go(id: SPObservationID): Task[Boolean] = 
    cancelRef.get.map(!_(id))

  def read(oid: SPObservationID): SeqexecFailure \/ ConfigSequence =
      SeqExecService.client(loc).sequence(oid).leftMap(SeqexecFailure.ODBSeqError(_))

  def sequence(sequenceConfig: ConfigSequence):  SeqexecFailure \/ (Set[System], List[Step.Step]) = {
    val a = sequenceConfig.getAllSteps.toList.map(Step.step).sequenceU
    a map { _.unzip match {
      case (l, s) => (l.suml, s)
    }}
  }

  // todo: it is an error to run a sequence with an existing ExecState != s
  private def runSeq(id: SPObservationID, s: ExecState): Task[(ExecState, NonEmptyList[SeqexecFailure] \/ Unit)] =
    cancelRef.modify(_ - id) *> recordState(id)(s) *> run(go(id), recordState(id))(s)

  def start(id: SPObservationID): SeqexecFailure \/ Unit = for {
    a <- read(id)
    b <- start(id, a)
  } yield b.runAsync(_ => ())

  private def start(id: SPObservationID, sequenceConfig: ConfigSequence): SeqexecFailure \/ Task[(ExecState, NonEmptyList[SeqexecFailure] \/ Unit)] = {
    sequence(sequenceConfig) map {
      case (s, l) =>
        // TODO: We have the set of systems used by the sequence. Check that they are available.
        runSeq(id, ExecState.initial(l))
    }
  }

  def continue(id: SPObservationID): SeqexecFailure \/ Unit = continueTask(id) map (_.runAsync(???))
  private def continueTask(id: SPObservationID): SeqexecFailure \/ Task[(Option[ExecState], NonEmptyList[SeqexecFailure] \/ Unit)] =
    getState(id).run map {runSeq(id, _).map(_.leftMap(_.some))}

  def state(id: SPObservationID): SeqexecFailure \/ ExecState = getState(id).run

  private def getState(id: SPObservationID): Task[SeqexecFailure \/ ExecState] =
    stateRef.get.map(_.get(id) \/> InvalidOp("Sequence has not started."))

  def stop(id: SPObservationID): SeqexecFailure \/ Unit = stopTask(id).run

  private def stopTask(id: SPObservationID): Task[SeqexecFailure \/ Unit] =
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

object ExecutorImpl extends ExecutorImpl(TaskRef.newTaskRef[Set[SPObservationID]](Set.empty).run, TaskRef.newTaskRef[Map[SPObservationID, Executor.ExecState]](Map.empty).run)
