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

object Executor { self =>
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
      EitherT[ExecAction, NonEmptyList[SeqexecFailure], A](self.fail(e))

    def liftA[A](a: ExecAction[A]): ExecActionF[A] =
      EitherT[ExecAction, NonEmptyList[SeqexecFailure], A](a.map(_.right))

    def liftE[A](a: ExecAction[NonEmptyList[SeqexecFailure] \/ A]): ExecActionF[A] =
      EitherT[ExecAction, NonEmptyList[SeqexecFailure], A](a)

    // Execute the next step, if any
    def stepT(saveState: ExecState => Task[Unit]): ExecActionF[StepResult] =
      for {
        st <- liftA(gets(_.nextStep))
        ds <- st.cata(t => liftE(liftT(t.run)), fail(Unexpected("No current step")))
        _  <- liftA(modify(s => s.ok(ds))) // only happens on success above
        _  <- liftA(get) >>= saveState.map(t => liftA(liftT(t)))
      } yield ds

    // Combine our boolean predicates ... we keep going as long as there is a next step AND the
    // user hasn't pressed the stop button
    def continue(go: Task[Boolean]): ExecActionF[Boolean] =
      (liftA(liftT(go)) |@| liftA(gets(_.nextStep.nonEmpty)))(_ && _)

    // Run to completion, or to error, as long as not cancelled or complete
    def runT(go: Task[Boolean], saveState: ExecState => Task[Unit]): ExecActionF[Unit] =
      stepT(saveState).whileM_(continue(go)) // cool eh?

  }

  // Execute the next step, if any
  def step(saveState: ExecState => Task[Unit]): ExecAction[NonEmptyList[SeqexecFailure] \/ StepResult] =
    WithEitherT.stepT(saveState).run

  // Run to completion, or to error, as long as not cancelled
  def run(go: Task[Boolean], saveState: ExecState => Task[Unit]): ExecAction[NonEmptyList[SeqexecFailure] \/ Unit] =
    WithEitherT.runT(go, saveState).run

  // Skip some number of steps
  def skip(n: Int): ExecAction[Unit] =
    modify(_.skip(n))

  // Get the current position in the sequence; (2, 4) means we're on step 2 of 4
  def position: ExecAction[(Int, Int)] =
    gets { case ExecState(cs, ss) => (cs.length + 1, cs.length + ss.length) }

}

case class StepResult(configResults: List[ConfigResult], observeResult: ObserveResult)

