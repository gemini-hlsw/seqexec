// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats._
import cats.data.StateT
import cats.effect.IO
import cats.implicits._
import seqexec.engine.Event._
import seqexec.engine.Result.{PartialVal, PauseContext, RetVal}
import seqexec.model.{ClientID, SequenceState}
import fs2.Stream
import gem.Observation
import monocle.{Lens, Optional}
import monocle.macros.Lenses
import monocle.function.Index.index
import mouse.boolean._
import org.log4s.getLogger

import scala.concurrent.ExecutionContext

class Engine[D, U](stateL: Lens[D, Engine.State]) {

  class ConcreteTypes extends Engine.Types {
    override type StateType = D
    override type EventData = U
  }
  type EventType = Event[ConcreteTypes]
  type ResultType = EventResult[ConcreteTypes]
  type UserEventType = UserEvent[ConcreteTypes]

  /**
    * Type constructor where all Seqexec side effect are managed.
    *
    * It's named `Handle` after `fs2.Handle` in order to give a hint in a future
    * migration.
    */
  type Handle[A] = HandleStateT[IO, A]
  // Helper alias to facilitate lifting.
  type HandleStateT[M[_], A] = StateT[M, D, A]

  /*
   * HandleP is a Stream which has as a side effect a State machine inside a IO, which can produce other
   * Streams as output.
   *
   * Its type parameters are:
   * A: Type of the output (usually Unit)
   * D: Type of the user data included in the state machine state.
   *
   * Making it final causes an error: "The outer reference in this type test cannot be checked at run time"
   */
  case class HandleP[A](run: Handle[(A, Option[Stream[IO, EventType]])])
  object HandleP {
    def fromStream(p: Stream[IO, EventType]): HandleP[Unit] = {
      HandleP[Unit](Applicative[Handle].pure[(Unit, Option[Stream[IO, EventType]])](((), Some(p))))
    }
  }

  implicit def handlePInstances: Monad[HandleP] = new Monad[HandleP] {
    private def concatOpP[F[_]](op1: Option[Stream[F, EventType]],
                          op2: Option[Stream[F, EventType]]): Option[Stream[F, EventType]] = (op1, op2) match {
      case (None, None)         => None
      case (Some(p1), None)     => Some(p1)
      case (None, Some(p2))     => Some(p2)
      case (Some(p1), Some(p2)) => Some(p1 ++ p2)
    }

    override def pure[A](a: A): HandleP[A] = HandleP(Applicative[Handle].pure((a, None)))

    override def flatMap[A, B](fa: HandleP[A])(f: A => HandleP[B]): HandleP[B] = HandleP[B](
      fa.run.flatMap {
        case (a, op1) => f(a).run.map{
          case (b, op2) => (b, concatOpP(op1, op2))
        }
      }
    )

    // Kudos to @tpolecat
    def tailRecM[A, B](a: A)(f: A => HandleP[Either[A, B]]): HandleP[B] = {
      // We don't really care what this type is
      type Unused = Option[Stream[IO, EventType]]

      // Construct a StateT that delegates to IO's tailRecM
      val st: StateT[IO, D, (B, Unused)] =
        StateT { s =>
          Monad[IO].tailRecM[(D, A), (D, (B, Unused))]((s, a)) {
            case (s, a) =>
              f(a).run.run(s).map {
                case (sʹ, (Left(a), _))  => Left((sʹ, a))
                case (sʹ, (Right(b), u)) => Right((sʹ, (b, u)))
              }
          }
        }

      // Done
      HandleP(st)
    }
  }

  implicit class HandleToHandleP[A](self: Handle[A]) {
    def toHandleP: HandleP[A] = HandleP(self.map((_, None)))
  }

  /**
    * Changes the `Status` and returns the new `Queue.State`.
    */
  private def switch(id: Observation.Id)(st: SequenceState): HandleP[Unit] =
    modifyS(id)(s => Sequence.State.status.set(st)(s))

  private def start(id: Observation.Id, clientId: ClientID, userCheck: D => Boolean): HandleP[Unit] =
    getS(id).flatMap {
      case Some(seq) =>
        // No resources being used by other running sequences
        if (seq.status.isIdle || seq.status.isError)
          get.flatMap { st =>
            if (userCheck(st))
              putS(id)(Sequence.State.status.set(SequenceState.Running.init)(seq.skips.getOrElse(seq).rollback)) *>
                send(Event.executing(id))
            // cannot run sequence
            else send(busy(id, clientId))
          }
        else unit
      case None      => unit
    }

  private def pause(id: Observation.Id): HandleP[Unit] = modifyS(id)(Sequence.State.userStopSet(true))

  private def cancelPause(id: Observation.Id): HandleP[Unit] = modifyS(id)(Sequence.State.userStopSet(false))

  /**
    * Load a Sequence
    * If the sequence already exists, it only updates the pending steps.
    */
  def load(id: Observation.Id, seq: Sequence): Endo[D] =
    stateL.modify(st =>
      st.copy(sequences = st.sequences.get(id).map(t => st.sequences.updated(id, Sequence.State.reload(seq.steps, t))
        ).getOrElse(st.sequences.updated(id, Sequence.State.init(seq))))
    )

  def unload(id: Observation.Id): Endo[D] =
    stateL.modify(
      st => st.copy(sequences =
        st.sequences.get(id).map(t =>
          if (Sequence.State.isRunning(t)) st.sequences
          else st.sequences - id
        ).getOrElse(st.sequences)
      )
    )

  /**
    * Refresh the steps executions of an existing sequence
    * @param id sequence identifier
    * @param steps List of new steps definitions
    * @return
    */
  def update(id: Observation.Id, steps: List[Step]): Endo[D] =
    (stateL ^|-? Engine.State.sequenceState(id)).modify(_.update(steps.map(_.executions)))

  /**
    * Adds the current `Execution` to the completed `Queue`, makes the next
    * pending `Execution` the current one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  private def next(id: Observation.Id): HandleP[Unit] =
    getS(id).flatMap(
      _.map { seq =>
        if (Sequence.State.anyStopRequested(seq)) {
          seq.next match {
            case None =>
              send(finished(id))
            // Final State
            case Some(qs: Sequence.State.Final) =>
              putS(id)(qs) *> send(finished(id))
            // Execution completed
            case Some(qs) =>
              putS(id)(qs) *> switch(id)(SequenceState.Idle)
          }
        } else if (Sequence.State.isRunning(seq)) {
          seq.next match {
            // Empty state
            case None =>
              send(finished(id))
            // Final State
            case Some(qs: Sequence.State.Final) =>
              putS(id)(qs) *> send(finished(id))
            // Execution completed. Check breakpoint here
            case Some(qs) =>
              putS(id)(qs) *> (if (qs.getCurrentBreakpoint) {
                switch(id)(SequenceState.Idle) *> send(breakpointReached(id))
              } else send(executing(id)))
          }
        } else unit
      }.getOrElse(unit)
    )

  /**
    * Executes all actions in the `Current` `Execution` in parallel. When all are done it emits the `Executed` event.
    * It also updates the `State` as needed.
    */
  // Send the expected event when the `Action` is executed
  // It doesn't catch run time exceptions. If desired, the Action has to do it itself.
  private def act(id: Observation.Id, t: (IO[Result], Int)): Stream[IO, EventType] = t match {
    case (gen, i) =>
      Stream.eval(gen).flatMap {
        case r@Result.OK(_)         => Stream(completed(id, i, r))
        case r@Result.Partial(_, c) => Stream(partial(id, i, r)) ++ act(id, (c, i))
        case e@Result.Error(_)      => Stream(failed(id, i, e))
        case r@Result.Paused(_)     => Stream(paused(id, i, r))
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  private def execute(id: Observation.Id)(implicit ec: ExecutionContext): HandleP[Unit] = {
    get.flatMap(st => stateL.get(st).sequences.get(id).map {
      case seq@Sequence.State.Final(_, _) =>
        // The sequence is marked as completed here
        putS(id)(seq) *> send(finished(id))
      case seq                            =>
        val u: List[Stream[IO, EventType]] = seq.current.actions.map(_.gen).zipWithIndex.map(x => act(id, x))
        val v: Stream[IO, EventType] = Stream.emits(u).join(u.length)
        val w: List[HandleP[Unit]] = seq.current.actions.indices.map(i => modifyS(id)(_.start(i))).toList
        w.sequence *> HandleP.fromStream(v)
    }.getOrElse(unit)
    )
  }

  private def getState(f: D => Option[Stream[IO, EventType]]): HandleP[Unit] =
    get.flatMap(s => HandleP[Unit](f(s).pure[Handle].map(((), _))))

  private def actionStop(id: Observation.Id, f: D => Option[Stream[IO, EventType]]): HandleP[Unit] =
    getS(id).flatMap(_.map(s => if (Sequence.State.isRunning(s)) HandleP(StateT[IO, D, (Unit, Option[Stream[IO, EventType]])](st => IO((st, ((), f(st)))))) *> modifyS(id)(Sequence.State.internalStopSet(true)) else unit).getOrElse(unit))

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exist it does nothing.
    */
  private def complete[R <: RetVal](id: Observation.Id, i: Int, r: Result.OK[R]): HandleP[Unit] = modifyS(id)(_.mark(i)(r)) *>
    getS(id).flatMap(_.flatMap(
      _.current.execution.forall(Action.completed).option(HandleP.fromStream(Stream(executed(id))))
    ).getOrElse(unit))

  private def partialResult[R <: PartialVal](id: Observation.Id, i: Int, p: Result.Partial[R]): HandleP[Unit] = modifyS(id)(_.mark(i)(p))

  def actionPause[C <: PauseContext](id: Observation.Id, i: Int, p: Result.Paused[C]): HandleP[Unit] = modifyS(id)(s => Sequence.State.internalStopSet(false)(s).mark(i)(p))

  private def actionResume(id: Observation.Id, i: Int, cont: IO[Result]): HandleP[Unit] = getS(id).flatMap(_.map { s =>
    if (Sequence.State.isRunning(s) && s.current.execution.lift(i).exists(Action.paused))
      modifyS(id)(_.start(i)) *> HandleP.fromStream(act(id, (cont, i)))
    else unit
  }.getOrElse(unit))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  private def fail(id: Observation.Id)(i: Int, e: Result.Error): HandleP[Unit] =
    modifyS(id)(_.mark(i)(e)) *>
      switch(id)(SequenceState.Failed(e.msg))

  private def logError(e: Result.Error): HandleP[Unit] = Logger.error(e.errMsg.getOrElse(e.msg))

  /**
    * Ask for the current Handle `Status`.
    */
  // private def status(id: Observation.Id): HandleP[Option[SequenceState], D] = inspect(_.sequences.get(id).map(_.status))

  // You shouldn't need to import this but if you do you could use the qualified
  // import: `engine.Logger`
  private object Logger {

    private val logger = getLogger

    /**
      * Log info lifted into Handle.
      */
    def info(msg: => String): HandleP[Unit] = pure((logger.info(msg), None)).void

    /**
      * Log warning lifted into Handle.
      */
    def warning(msg: => String): HandleP[Unit] = pure((logger.warn(msg), None)).void

    /**
      * Log debug lifted into Handle.
      */
    def debug(msg: => String): HandleP[Unit] = pure((logger.debug(msg), None)).void

    /**
      * Log error lifted into Handle
      */
    def error(msg: => String): HandleP[Unit] = pure((logger.error(msg), None)).void

  }

  /**
    * Enqueue `Event` in the Handle.
    */
  private def send(ev: EventType): HandleP[Unit] = HandleP.fromStream(Stream(ev))

  /**
    * Main logical thread to handle events and produce output.
    */
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  private def run(ev: EventType)(implicit ec: ExecutionContext): HandleP[ResultType] = {
    def handleUserEvent(ue: UserEventType): HandleP[ResultType] = ue match {
      case Start(id, _, clid, userCheck) => Logger.debug(s"Engine: Start requested for sequence ${id.format}") *> start(id, clid, userCheck) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case Pause(id, _)                  => Logger.debug(s"Engine: Pause requested for sequence ${id.format}") *> pause(id) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case CancelPause(id, _)            => Logger.debug(s"Engine: Pause canceled for sequence ${id.format}") *> cancelPause(id) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case Breakpoint(id, _, step, v)    => Logger.debug(s"Engine: breakpoint changed for sequence ${id.format} and step $step to $v") *>
        modifyS(id)(_.setBreakpoint(step, v)) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case SkipMark(id, _, step, v)      => Logger.debug(s"Engine: skip mark changed for sequence ${id.format} and step $step to $v") *>
        modifyS(id)(_.setSkipMark(step, v)) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case Poll(_)                       => Logger.debug("Engine: Polling current state") *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case GetState(f)                   => getState(f) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case ModifyState(f)                => userModify(f).map(r => UserCommandResponse[ConcreteTypes](ue, EventResult.Ok, Some(r)))
      case ActionStop(id, f)             => Logger.debug("Engine: Action stop requested") *> actionStop(id, f) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case ActionResume(id, i, cont)     => Logger.debug("Engine: Action resume requested") *> actionResume(id, i, cont) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case LogDebug(msg)                 => Logger.debug(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case LogInfo(msg)                  => Logger.info(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case LogWarning(msg)               => Logger.warning(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
      case LogError(msg)                 => Logger.error(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    }

    def handleSystemEvent(se: SystemEvent): HandleP[ResultType] = se match {
      case Completed(id, i, r)     => Logger.debug("Engine: Action completed") *> complete(id, i, r) *> pure(SystemUpdate(se, EventResult.Ok))
      case PartialResult(id, i, r) => Logger.debug("Engine: Partial result") *> partialResult(id, i, r) *> pure(SystemUpdate(se, EventResult.Ok))
      case Paused(id, i, r)        => Logger.debug("Engine: Action paused") *> actionPause(id, i, r) *> pure(SystemUpdate(se, EventResult.Ok))
      case Failed(id, i, e)        => logError(e) *> fail(id)(i, e) *> pure(SystemUpdate(se, EventResult.Ok))
      case Busy(id, _)             => Logger.warning(s"Cannot run sequence ${id.format} because required systems are in use.") *> pure(SystemUpdate(se, EventResult.Ok))
      case BreakpointReached(_)    => Logger.debug("Engine: Breakpoint reached") *> pure(SystemUpdate(se, EventResult.Ok))
      case Executed(id)            => Logger.debug("Engine: Execution completed") *> next(id) *> pure(SystemUpdate(se, EventResult.Ok))
      case Executing(id)           => Logger.debug("Engine: Executing") *> execute(id) *> pure(SystemUpdate(se, EventResult.Ok))
      case Finished(id)            => Logger.debug("Engine: Finished") *> switch(id)(SequenceState.Completed) *> pure(SystemUpdate(se, EventResult.Ok))
      case Null                    => pure(SystemUpdate(se, EventResult.Ok))
    }

    (ev match {
      case EventUser(ue)   => handleUserEvent(ue)
      case EventSystem(se) => handleSystemEvent(se)
    })
  }

  /** Traverse a process with a stateful computation. */
  // input, stream of events
  // initalState: state
  // f takes an event and the current state, it produces a new state, a new value B and more actions
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def mapEvalState[A, S, B](input: Stream[IO, A], initialState: S, f: (A, S) => IO[(S, B, Stream[IO, A])])(implicit ec: ExecutionContext): Stream[IO, B] = {
    Stream.eval(fs2.async.unboundedQueue[IO, Stream[IO, A]]).flatMap { q =>
      Stream.eval_(q.enqueue1(input)) ++
        q.dequeue.joinUnbounded.evalMapAccumulate(initialState) { (s, a) =>
          f(a, s).flatMap {
            case (ns, b, st) =>
              q.enqueue1(st) >> IO.pure((ns, b))
          }
        }.map(_._2)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AnyVal", "org.wartremover.warts.ImplicitParameter"))
  private def runE(ev: EventType, s: D)(implicit ec: ExecutionContext): IO[(D, (ResultType, D), Stream[IO, EventType])] =
    run(ev).run.run(s).map {
      case (si, (r, p)) =>
        (si, (r, si), p.getOrElse(Stream.empty))
    }

  @SuppressWarnings(Array("org.wartremover.warts.AnyVal", "org.wartremover.warts.ImplicitParameter"))
  def process(input: Stream[IO, EventType])(qs: D)(implicit ec: ExecutionContext): Stream[IO, (ResultType, D)] =
    mapEvalState[EventType, D, (ResultType, D)](input, qs, runE)

  private def userModify(f: D => (D, U)): HandleP[U] = StateT[IO, D, U]( st => IO(f(st)) ).toHandleP

  // Functions for type bureaucracy

  private def pure[A](a: A): HandleP[A] = Applicative[HandleP].pure(a)

  private val unit: HandleP[Unit] = pure(())

  private val get: HandleP[D] =
    StateT.get[IO, D].toHandleP

  private def inspect[A](f: D => A): HandleP[A] =
    StateT.inspect[IO, D, A](f).toHandleP

  private def modify(f: D => D): HandleP[Unit] =
    StateT.modify[IO, D](f).toHandleP

  private def getS(id: Observation.Id): HandleP[Option[Sequence.State]] = get.map(stateL.get(_).sequences.get(id))

  private def getSs[A](id: Observation.Id)(f: Sequence.State => A): HandleP[Option[A]] =
    inspect(stateL.get(_).sequences.get(id).map(f))

  private def modifyS(id: Observation.Id)(f: Sequence.State => Sequence.State): HandleP[Unit] =
    modify((stateL ^|-? Engine.State.sequenceState(id)).modify(f))

  private def putS(id: Observation.Id)(s: Sequence.State): HandleP[Unit] =
    modify((stateL ^|-? Engine.State.sequenceState(id)).set(s))

  // For debugging
  def printSequenceState(id: Observation.Id): HandleP[Unit] =
    getSs(id)((qs: Sequence.State) => StateT.liftF(IO.pure(println(qs)))).void // scalastyle:ignore

}

@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
object Engine {

  @Lenses
  final case class State(sequences: Map[Observation.Id, Sequence.State])

  object State {
    def empty: State = State(Map.empty)
    private def atSequence(id: Observation.Id): Optional[Map[Observation.Id, Sequence.State], Sequence.State] = index(id)
    def sequenceState[D](id: Observation.Id): Optional[State, Sequence.State] = State.sequences ^|-? atSequence(id)
  }

  abstract class Types {
    type StateType
    type EventData
  }

}
