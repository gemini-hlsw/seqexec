package edu.gemini.seqexec.server

import edu.gemini.seqexec.odb.SeqExecService
import edu.gemini.spModel.core.Peer

import scalaz.{\/, _}
import Scalaz._
import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.Executor._
import edu.gemini.seqexec.server.SeqexecFailure._
import edu.gemini.spModel.config2.ConfigSequence

import scalaz.concurrent.Task
import scalaz.stream.Process

/** An executor that maintains state in a pair of `TaskRef`s. */
class ExecutorImpl private (cancelRef: TaskRef[Set[SPObservationID]], stateRef: TaskRef[Map[SPObservationID, Executor.ExecState]]) {
  type ExecutionFailures = NonEmptyList[SeqexecFailure] \/ Unit
  type ExecutionResult = (ExecState, ExecutionFailures)

  private var loc = new Peer("localhost", 8443, null)

  def host(): Peer = loc
  def host(l: Peer): Unit = { loc = l }

  private def recordState(id: SPObservationID)(s: ExecState): Task[Unit] =
    // This marks the current state for observation id
    stateRef.modify(_ + (id -> s))

  private def go(id: SPObservationID): Task[Boolean] =
    // It seems this will check cancelRef and return a Task with value true if the observation has not been cancelled
    cancelRef.get.map(!_(id))

  /**
    * Reads a sequence returning either a failure or the configuration for the sequence
    */
  def read(oid: SPObservationID): SeqexecFailure \/ ConfigSequence =
    SeqExecService.client(loc).sequence(oid).leftMap(SeqexecFailure.ODBSeqError)

  /**
    * Reads a sequence returning either a failure or a list of systems and their steps
    */
  def sequence(sequenceConfig: ConfigSequence): SeqexecFailure \/ (Set[System], List[Step.Step]) = {
    val a = sequenceConfig.getAllSteps.toList.map(Step.step).sequenceU
    a.map { _.unzip match {
      case (l, s) => (l.suml, s)
    }}
  }

  // todo: it is an error to run a sequence with an existing ExecState != s
  private def runSeq(id: SPObservationID, s: ExecState): Task[ExecutionResult] =
    cancelRef.modify(_ - id) *> recordState(id)(s) *> run(go(id), recordState(id))(s)

  /**
    * Start executing a sequence
    */
  def start(id: SPObservationID): SeqexecFailure \/ Unit = for {
    a <- read(id)
    b <- start(id, a)
  } yield b.unsafePerformAsync(_ => ())

  private def start(id: SPObservationID, sequenceConfig: ConfigSequence): SeqexecFailure \/ Task[ExecutionResult] = {
    sequence(sequenceConfig).map {
      case (_, l) =>
        // TODO: We have the set of systems used by the sequence. Check that they are available.
        runSeq(id, ExecState.initial(l))
    }
  }

  /**
    * Continue a sequence, unimplemented
    */
  def continue(id: SPObservationID): SeqexecFailure \/ Unit = continueTask(id) map (_.unsafePerformAsync(???))

  private def continueTask(id: SPObservationID): SeqexecFailure \/ Task[(Option[ExecState], ExecutionFailures)] =
    getState(id).unsafePerformSync.map {runSeq(id, _).map(_.leftMap(_.some))}

  /**
    * Return the current state of a sequence
    */
  def state(id: SPObservationID): SeqexecFailure \/ ExecState = getState(id).unsafePerformSync

  private def getState(id: SPObservationID): Task[SeqexecFailure \/ ExecState] =
    stateRef.get.map(_.get(id) \/> InvalidOp("Sequence has not started."))

  /**
    * Stop execution
    */
  def stop(id: SPObservationID): SeqexecFailure \/ Unit = stopTask(id).unsafePerformSync

  private def stopTask(id: SPObservationID): Task[SeqexecFailure \/ Unit] =
    getState(id) >>= {
      case \/-(s) => cancelRef.modify(s => s + id).as(\/-(()))
      case -\/(e) => Task.now(-\/(e))
    }

  def stateDescription(state: ExecState): (String, List[String]) = {
    ("Completed " + state.completed.length + " steps out of " + (state.completed.length + state.remaining.length),
      state.completed.zipWithIndex.map(a => (a._1, a._2 + 1)) map {
        case (Ok(StepResult(_, ObserveResult(label))), idx) => s"Step $idx completed with label $label"
        case (Failed(result), idx) => s"Step $idx failed with error " + result.map(SeqexecFailure.explain).toList.mkString("\n", "\n", "")
        case (Skipped, idx) => s"Step $idx skipped"
      })
  }

  // This is a mock data source, but could be a Process representing results from a database
  def sequenceEvents: Process[Task, String] = {
    val stream: Process[Task, String] = Process.emitAll(List("Abc", "cd"))

    Process.emit(s"Starting stream intervals, taking results\n\n") ++ stream
  }

}

// Initialize with no cancelled observation and no observations in execution
object ExecutorImpl extends ExecutorImpl(TaskRef.newTaskRef[Set[SPObservationID]](Set.empty).unsafePerformSync, TaskRef.newTaskRef[Map[SPObservationID, Executor.ExecState]](Map.empty).unsafePerformSync)
