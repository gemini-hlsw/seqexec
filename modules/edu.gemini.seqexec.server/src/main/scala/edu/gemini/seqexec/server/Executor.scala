package edu.gemini.seqexec.server

import java.util.concurrent.{ScheduledExecutorService, ScheduledThreadPoolExecutor}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import edu.gemini.pot.sp.SPObservationID
import edu.gemini.seqexec.server.SeqexecFailure._
import edu.gemini.spModel.config2.{Config, ConfigSequence, ItemKey}
import edu.gemini.spModel.obscomp.InstConstants.INSTRUMENT_NAME_PROP
import edu.gemini.spModel.seqcomp.SeqConfigNames.INSTRUMENT_KEY

import scalaz._
import Scalaz._

import scalaz.concurrent.Task

import scala.collection.concurrent.{TrieMap, Map}

/**
 * Created by jluhrs on 4/28/15.
 */

object Step {
  type Step = EitherT[Task, NonEmptyList[SeqexecFailure], StepResult]

  def parConfig(config: List[Task[SeqexecFailure \/ ConfigResult]]):
  EitherT[Task, NonEmptyList[SeqexecFailure], List[ConfigResult]] =
    EitherT(Nondeterminism[Task].gather(config).map(_.map(_.validationNel).sequenceU.disjunction))

  def step(config: List[Task[SeqexecFailure \/ ConfigResult]], observe: Task[SeqexecFailure \/ ObserveResult]): Step =
    for {
      p <- parConfig(config)
      q <- EitherT(observe.map(_.leftMap(NonEmptyList(_))))
    } yield StepResult(p, q)

  def step(config: Config): Step = {
    val instName = config.getItemValue(new ItemKey(INSTRUMENT_KEY, INSTRUMENT_NAME_PROP))
    val instrument = instName match {
      case GMOS_S.name => Some(GMOS_S)
      case _ => None
    }

    instrument map { a => {
        val systems = List(TCS, a)
        step(systems.map(_.configure(config)), a.observe(config))
      }
    } getOrElse EitherT(Task(NonEmptyList[SeqexecFailure](UnrecognizedInstrument(instName.toString)).left))
  }

}

object Executor {
  import Step._

  sealed trait Completion
  case class  Ok(result: StepResult) extends Completion
  case class  Failed(result: NonEmptyList[SeqexecFailure]) extends Completion
  case object Skipped extends Completion

  implicit val executor: ScheduledExecutorService = new ScheduledThreadPoolExecutor(10)
  def shutdown(): Unit = executor.shutdown

  // Our current exec state is a list of completed steps and a list of remaining steps
  case class ExecState(completed: List[Completion], remaining: List[Step]) {
    def skip(n: Int): ExecState = {
      val (skipped, rest) = remaining.splitAt(n)
      ExecState(completed ++ skipped.as(Skipped), rest)
    }
    def ok(result: StepResult): ExecState =
      ExecState(completed :+ Ok(result), remaining.tail) // not quite right
    def nextStep: Option[Step] = remaining.headOption
  }
  object ExecState {
    def initial(seq: List[Step]): ExecState = apply(Nil, seq)
    def empty: ExecState = initial(Nil)
  }
    // The type of execution actions.
  type ExecAction[+A] = StateT[Task, ExecState, A]

  // We now have the problem of lifting normal State/Task values into this transformer. The next
  // few definitions do this.

  // State accessors, lifted into ExecAction
  def get: ExecAction[ExecState] = State.get.lift[Task]
  def gets[A](f: ExecState => A): ExecAction[A] = State.gets(f).lift[Task]

  // State mutators; private to discourage messing with the state outside of this module
  private def modify(f: ExecState => ExecState): ExecAction[Unit] = State.modify(f).lift[Task]
  private def put(s: ExecState): ExecAction[Unit] = State.put(s).lift[Task]

  // Lift a normal task into ExecAction
  def liftT[A](a: Task[A]): ExecAction[A] =
    a.liftM[({ type λ[μ[+_], β] = StateT[μ ,ExecState, β] })#λ]

  // Lift a failing value
  def fail[A](e: SeqexecFailure): ExecAction[NonEmptyList[SeqexecFailure] \/ A] =
    NonEmptyList(e).left[A].point[ExecAction]

  // Actions of the form ExecAction[NonEmptyList[SeqexecFailure] \/ StepResult] are more
  // easily expressed as EitherT[ExecAction, NonEmptyList[SeqexecFailure], StepResult] so
  // we do that here, and provide unfolded versions for public consumption. This is a two-level
  // monad transformer, which starts to get awkward because inference gets weaker.
  object WithEitherT {

    // This is isomorphic to the StateT above, but gives fast-fail semantics
    private type ExecActionF[+A] = EitherT[ExecAction, NonEmptyList[SeqexecFailure], A]

    def fail[A](e: SeqexecFailure): ExecActionF[A] =
      EitherT[ExecAction, NonEmptyList[SeqexecFailure], A](Executor.fail(e))

    def liftA[A](a: ExecAction[A]): ExecActionF[A] =
      EitherT[ExecAction, NonEmptyList[SeqexecFailure], A](a.map(_.right))

    def liftE[A](a: ExecAction[NonEmptyList[SeqexecFailure] \/ A]): ExecActionF[A] =
      EitherT[ExecAction, NonEmptyList[SeqexecFailure], A](a)

    // Execute the next step, if any
    def stepT(saveState: ExecState => ExecState): ExecActionF[StepResult] =
      for {
        st <- liftA(gets(_.nextStep))
        ds <- st.cata(t => liftE(liftT(t.run)), fail(Unexpected("No current step")))
        _  <- liftA(modify(s => saveState(s.ok(ds)))) // only happens on success above
      } yield ds

    // Combine our boolean predicates ... we keep going as long as there is a next step AND the
    // user hasn't pressed the stop button
    def continue(go: Task[Boolean]): ExecActionF[Boolean] =
      (liftA(liftT(go)) |@| liftA(gets(_.nextStep.nonEmpty)))(_ && _)


    // Run to completion, or to error, as long as not cancelled or complete
    def runT(go: Task[Boolean], saveState: ExecState => ExecState): ExecActionF[Unit] =
      stepT(saveState).whileM_(continue(go)) // cool eh?

  }

  // Execute the next step, if any
  def step(saveState: ExecState => ExecState): ExecAction[NonEmptyList[SeqexecFailure] \/ StepResult] =
    WithEitherT.stepT(saveState).run

  // Run to completion, or to error, as long as not cancelled
  def run(go: Task[Boolean], saveState: ExecState => ExecState): ExecAction[NonEmptyList[SeqexecFailure] \/ Unit] =
    WithEitherT.runT(go, saveState).run

  // Skip some number of steps
  def skip(n: Int): ExecAction[Unit] =
    modify(_.skip(n))

  // Get the current position in the sequence; (2, 4) means we're on step 2 of 4
  def position: ExecAction[(Int, Int)] =
    gets { case ExecState(cs, ss) => (cs.length + 1, cs.length + ss.length) }

  def recordState(ref: AtomicReference[ExecState])(state: ExecState): ExecState = {
    ref.set(state)
    state
  }

  def go(stop: AtomicBoolean): Task[Boolean] = Task.delay(!stop.get())

  case class Sequence(stateRef: AtomicReference[ExecState], stopRef: AtomicBoolean)

  val sequences: Map[SPObservationID, Sequence] = TrieMap.empty

  def startSequence(stateRef: AtomicReference[ExecState], stopRef: AtomicBoolean): Task[(ExecState, NonEmptyList[SeqexecFailure] \/ Unit)] = {
    stopRef.set(false)
    Executor.run(go(stopRef), recordState(stateRef))(stateRef.get)
  }

  def newSequence(sequenceConfig: ConfigSequence): Sequence = new Sequence(
      new AtomicReference(ExecState.initial(sequenceConfig.getAllSteps.toList.map(Step.step))),
      new AtomicBoolean(false))

  def startCmd(obsId: SPObservationID, sequenceConfig: ConfigSequence): Unit = {
    val s = sequences.getOrElse(obsId, {
      val t = newSequence(sequenceConfig)
      sequences += ( (obsId, t) )
      t
    })

    startSequence(s.stateRef, s.stopRef).runAsync(_.void)
  }

  def continueCmd(obsId: SPObservationID): TrySeq[Unit] = sequences.get(obsId) match {
    case Some(Sequence(a,b)) => TrySeq(startSequence(a, b).runAsync(_.void))
    case _                  => TrySeq.fail(InvalidOp(s"Attempt to continue unexecuted sequence $obsId"))
  }

  def getStateCmd(obsId: SPObservationID): TrySeq[ExecState] = sequences.get(obsId) match {
    case Some(Sequence(a,_)) => TrySeq(a.get)
    case _                   => TrySeq.fail(InvalidOp(s"Attempt to retrieve execution state from unexecuted sequence $obsId"))
  }

  def stopCmd(obsId: SPObservationID): TrySeq[Unit] = sequences.get(obsId) match {
      case Some(Sequence(_,b)) => TrySeq(b.set(true))
      case _                   => TrySeq.fail(InvalidOp(s"Attempt to stop unexecuted sequence $obsId"))
    }

  def stateDescription(state: ExecState): String =  {
    "Completed " + state.completed.length + " steps out of " + (state.completed.length + state.remaining.length) +
      ( state.completed.zipWithIndex.map(a => (a._1, a._2+1)) map {
        case (Ok(StepResult(_,ObserveResult(label))), idx) => s"Step $idx completed with label $label"
        case (Failed(result), idx) => s"Step $idx failed with error " + result.map(SeqexecFailure.explain).toList.mkString("\n", "\n", "")
        case (Skipped, idx)        => s"Step $idx skipped"
      }
    ).mkString("\n", "\n", "")
  }
}

case class StepResult(configResults: List[ConfigResult], observeResult: ObserveResult)

