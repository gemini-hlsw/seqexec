// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats._
import cats.data.StateT
import cats.effect.Concurrent
import cats.implicits._
import fs2.Stream
import gem.Observation
import monocle.Optional
import mouse.boolean._
import io.chrisdavenport.log4cats.Logger
import seqexec.engine.Event._
import seqexec.engine.EventResult.UserCommandResponse
import seqexec.engine.EventResult.SystemUpdate
import seqexec.engine.EventResult.Outcome
import seqexec.engine.Result.{PartialVal, RetVal}
import seqexec.engine.SystemEvent._
import seqexec.engine.UserEvent._
import seqexec.model.{ClientId, SequenceState, StepId}

class Engine[F[_]: MonadError[?[_], Throwable]: Logger, S, U](stateL: Engine.State[F, S]) {
  val L: Logger[F] = Logger[F]

  type EventType = Event[F, S, U]
  type ResultType = EventResult[U]
  type UserEventType = UserEvent[F, S, U]
  type HandleType[A] = Handle[F, S, EventType, A]

  /**
    * Changes the `Status` and returns the new `Queue.State`.
    */
  private def switch(id: Observation.Id)(st: SequenceState): HandleType[Unit] =
    modifyS(id)(Sequence.State.status.set(st))

  def start(id: Observation.Id, clientId: ClientId, userCheck: S => Boolean): Handle[F, S, Event[F, S, U], Unit] =
    getS(id).flatMap {
      case Some(seq) =>
        // No resources being used by other running sequences
        (get.flatMap { st =>
          if (userCheck(st)) {
            putS(id)(Sequence.State.status.set(SequenceState.Running.init)(seq.skips.getOrElse(seq).rollback)) *>
              send(Event.executing(id))
          // cannot run sequence
          } else {
            send(busy(id, clientId))
          }
        }).whenA(seq.status.isIdle || seq.status.isError)
      case None      => unit
    }

  /**
   * startFrom starts a sequence from an arbitrary step. It does it by marking all previous steps to be skipped and then
   * modifying the state sequence as if it was run.
   * If the requested step is already run or marked to be skipped, the sequence will start from the next runnable step
   */
  def startFrom(id: Observation.Id, step: StepId): HandleType[Unit] =
    getS(id).flatMap {
      case Some(seq) if (seq.status.isIdle || seq.status.isError) && seq.toSequence.steps.exists(_.id === step) =>
        val steps = seq.toSequence.steps.takeWhile(_.id =!= step).mapFilter(p => p.status.canRunFrom.option(p.id))
        val withSkips = steps.foldLeft[Sequence.State[F]](seq){ case (s, i) => s.setSkipMark(i, v = true) }
        putS(id)(
          Sequence.State.status.set(SequenceState.Running.init)(withSkips.skips.getOrElse(withSkips).rollback)
        ) *> send(Event.executing(id))
      case _ => unit
    }

  def pause(id: Observation.Id): HandleType[Unit] =
    modifyS(id)(Sequence.State.userStopSet(true))

  private def cancelPause(id: Observation.Id): HandleType[Unit] =
    modifyS(id)(Sequence.State.userStopSet(false))

  /**
    * Builds the initial state of a sequence
    */
  def load(seq: Sequence[F]): Sequence.State[F] = Sequence.State.init(seq)

  /**
    * Redefines an existing sequence. Changes the step actions, removes steps, adds new steps.
    */
  def reload(seq: Sequence.State[F], steps: List[Step[F]]): Sequence.State[F] =
    Sequence.State.reload(steps, seq)

  def startSingle(c: ActionCoords): HandleType[Outcome] = get.flatMap { st =>
    val x = for {
      seq <- stateL.sequenceStateIndex(c.sid).getOption(st)
      if (seq.status.isIdle || seq.status.isError) && !seq.getSingleState(c.actCoords).active
      act <- seq.getSingleAction(c.actCoords)
    } yield act.gen

    x.map(p =>
      modifyS(c.sid)(_.startSingle(c.actCoords)) *>
      Handle.fromStream[F, S, EventType](
        p.attempt.flatMap {
          case Right(r @ Result.OK(_))    =>
            Stream.emit(singleRunCompleted(c, r))
          case Right(e @ Result.Error(_)) =>
            Stream.emit(singleRunFailed(c, e))
          case Right(r)                   =>
            Stream.emit(singleRunFailed(c, Result.Error(s"Unhandled result for single run action: $r")))
          case Left(t: Throwable)         => Stream.raiseError[F](t)
        }
      ).as[Outcome](Outcome.Ok)
    ).getOrElse(pure[Outcome](Outcome.Failure))

  }

  private def completeSingleRun[V <: RetVal](c: ActionCoords, r: V): HandleType[Unit] =
    modifyS(c.sid)(_.completeSingle(c.actCoords, r))

  private def failSingleRun(c: ActionCoords, e: Result.Error): HandleType[Unit] =
    modifyS(c.sid)(_.failSingle(c.actCoords, e))

  /**
    * Tells if a sequence can be safely removed
    */
  def canUnload(id: Observation.Id)(st: S): Boolean =
    stateL.sequenceStateIndex(id).getOption(st).forall(!Sequence.State.isRunning(_))

  /**
    * Refresh the steps executions of an existing sequence. Does not add nor remove steps.
    * @param id sequence identifier
    * @param steps List of new steps definitions
    * @return
    */
  def update(id: Observation.Id, steps: List[Step[F]]): Endo[S] =
    stateL.sequenceStateIndex(id).modify(_.update(steps.map(_.executions)))

  /**
    * Adds the current `Execution` to the completed `Queue`, makes the next
    * pending `Execution` the current one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  private def next(id: Observation.Id): HandleType[Unit] =
    getS(id).flatMap(
      _.map { seq =>
        if (Sequence.State.anyStopRequested(seq)) {
          seq.next match {
            case None =>
              send(finished(id))
            // Final State
            case Some(qs: Sequence.State.Final[F]) =>
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
            case Some(qs: Sequence.State.Final[F]) =>
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
  private def act(id: Observation.Id, stepId: StepId, t: (Stream[F, Result[F]], Int))
  : Stream[F, EventType] = t match {
    case (gen, i) =>
      gen.takeThrough {
        case Result.Partial(_) => true
        case _                 => false
      }.attempt.flatMap {
        case Right(r@Result.OK(_))        => Stream.emit(completed(id, stepId, i, r))
        case Right(r@Result.OKStopped(_)) => Stream.emit(stopCompleted(id, stepId, i, r))
        case Right(r@Result.OKAborted(_)) => Stream.emit(aborted(id, stepId, i, r))
        case Right(r@Result.Partial(_))   => Stream.emit(partial(id, stepId, i, r))
        case Right(e@Result.Error(_))     => Stream.emit(failed(id, i, e))
        case Right(r@Result.Paused(_))    => Stream.emit(paused[F](id, i, r))
        case Left(t: Throwable)           => Stream.raiseError[F](t)
      }
  }

  private def execute(id: Observation.Id)(implicit ev: Concurrent[F]): HandleType[Unit] = {
    get.flatMap(st => stateL.sequenceStateIndex(id).getOption(st).map {
      case seq@Sequence.State.Final(_, _)  =>
        // The sequence is marked as completed here
        putS(id)(seq) *> send(finished(id))
      case seq@Sequence.State.Zipper(z, _, _) =>
        val stepId = z.focus.toStep.id
        val u: List[Stream[F, EventType]] =
          seq
            .current
            .actions
            .map(_.gen)
            .zipWithIndex
            .map(act(id, stepId, _))
        val v: Stream[F, EventType] = Stream.emits(u).parJoin(u.length)
        val w: List[HandleType[Unit]] =
          seq
            .current
            .actions
            .indices
            .map(i => modifyS(id)(_.start(i)))
            .toList
        w.sequence *> Handle.fromStream(v)
    }.getOrElse(unit) )
  }

  private def getState(f: S => Option[Stream[F, EventType]]): HandleType[Unit] =
    get.flatMap(s => Handle[F, S, EventType, Unit](f(s).pure[StateT[F, S, ?]].map(((), _))))

  private def actionStop(id: Observation.Id, f: S => Option[Stream[F, EventType]]): HandleType[Unit] =
    getS(id).flatMap(_.map{s =>
      (Handle(StateT[F, S, (Unit, Option[Stream[F, EventType]])](st => ((st, ((), f(st)))).pure[F])) *>
        modifyS(id)(Sequence.State.internalStopSet(true))).whenA(Sequence.State.isRunning(s))
    }.getOrElse(unit))

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exist it does nothing.
    */
  private def complete[R <: RetVal](id: Observation.Id, i: Int, r: Result.OK[R]): HandleType[Unit] = modifyS(id)(_.mark(i)(r)) *>
    getS(id).flatMap(_.flatMap(
      _.current.execution.forall(Action.completed).option(Handle.fromStream[F, S, EventType](Stream(executed(id))))
    ).getOrElse(unit))

  private def stopComplete[R <: RetVal](id: Observation.Id, i: Int, r: Result.OKStopped[R]): HandleType[Unit] = modifyS(id)(_.mark(i)(r)) *>
    getS(id).flatMap(_.flatMap(
      _.current.execution.forall(Action.completed).option(Handle.fromStream[F, S, EventType](Stream(executed(id))))
    ).getOrElse(unit))

  private def abort[R <: RetVal](id: Observation.Id, i: Int, r: Result.OKAborted[R]): HandleType[Unit] =
    modifyS(id)(_.mark(i)(r)) *>
      switch(id)(SequenceState.Aborted)

  private def partialResult[R <: PartialVal](id: Observation.Id, i: Int, p: Result.Partial[R]): HandleType[Unit] =
    modifyS(id)(_.mark(i)(p))

  def actionPause(id: Observation.Id, i: Int, p: Result.Paused[F]): HandleType[Unit] = modifyS(id)(s => Sequence.State.internalStopSet(false)(s).mark(i)(p))

  private def actionResume(id: Observation.Id, i: Int, cont: Stream[F, Result[F]])
  : HandleType[Unit] =
    getS(id).flatMap(_.collect {
      case s@Sequence.State.Zipper(z, _, _)
        if Sequence.State.isRunning(s) && s.current.execution.lift(i).exists(Action.paused) =>
          modifyS(id)(_.start(i)) *> Handle.fromStream(act(id, z.focus.toStep.id, (cont, i)))
  }.getOrElse(unit))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  private def fail(id: Observation.Id)(i: Int, e: Result.Error): HandleType[Unit] =
    modifyS(id)(_.mark(i)(e)) *>
      switch(id)(SequenceState.Failed(e.msg))

  private def logError(e: Result.Error): HandleType[Unit] = error(e.errMsg.getOrElse(e.msg))

  /**
    * Log info lifted into Handle.
    */
  private def info(msg: => String): HandleType[Unit] = Handle.liftF(L.info(msg))

  /**
    * Log warning lifted into Handle.
    */
  private def warning(msg: => String): HandleType[Unit] = Handle.liftF(L.warn(msg))

  /**
    * Log debug lifted into Handle.
    */
  private def debug(msg: => String): HandleType[Unit] = Handle.liftF(L.debug(msg))

  /**
    * Log error lifted into Handle
    */
  private def error(msg: => String): HandleType[Unit] = Handle.liftF(L.error(msg))

  /**
    * Enqueue `Event` in the Handle.
    */
  private def send(ev: EventType): HandleType[Unit] = Handle.fromStream(Stream(ev))

  private def handleUserEvent(ue: UserEventType): HandleType[ResultType] = ue match {
    case Start(id, _, clid, userCheck) => debug(s"Engine: Start requested for sequence ${id.format}") *> start(id, clid, userCheck) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Pause(id, _)                  => debug(s"Engine: Pause requested for sequence ${id.format}") *> pause(id) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case CancelPause(id, _)            => debug(s"Engine: Pause canceled for sequence ${id.format}") *> cancelPause(id) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Breakpoint(id, _, step, v)    => debug(s"Engine: breakpoint changed for sequence ${id.format} and step $step to $v") *>
      modifyS(id)(_.setBreakpoint(step, v)) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case SkipMark(id, _, step, v)      => debug(s"Engine: skip mark changed for sequence ${id.format} and step $step to $v") *>
      modifyS(id)(_.setSkipMark(step, v)) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case Poll(_)                       => debug("Engine: Polling current state") *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case GetState(f)                   => getState(f) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case ModifyState(f)                => f.map(r => UserCommandResponse[F, U](ue, Outcome.Ok, Some(r)))
    case ActionStop(id, f)             => debug("Engine: Action stop requested") *> actionStop(id, f) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case ActionResume(id, i, cont)     => debug("Engine: Action resume requested") *> actionResume(id, i, cont) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogDebug(msg, _)              => debug(msg) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogInfo(msg, _)               => info(msg) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogWarning(msg, _)            => warning(msg) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
    case LogError(msg, _)              => error(msg) *> pure(UserCommandResponse(ue, Outcome.Ok, None))
  }

  private def handleSystemEvent(se: SystemEvent[F])(implicit ci: Concurrent[F]): HandleType[ResultType] = se match {
    case Completed(id, _, i, r)        => debug(
      s"Engine: From sequence ${id.format}: Action completed ($r)") *> complete(id, i, r) *>
      pure(SystemUpdate(se, Outcome.Ok))
    case StopCompleted(id, _, i, r)    => debug(
      s"Engine: From sequence ${id.format}: Action completed with stop ($r)") *> stopComplete(id, i, r) *>
      pure(SystemUpdate(se, Outcome.Ok))
    case Aborted(id, _, i, r)    => debug(
      s"Engine: From sequence ${id.format}: Action completed with abort ($r)") *> abort(id, i, r) *>
      pure(SystemUpdate(se, Outcome.Ok))
    case PartialResult(id, _, i, r) => debug(
      s"Engine: From sequence ${id.format}: Partial result ($r)")  *> partialResult(id, i, r) *>
      pure(SystemUpdate(se, Outcome.Ok))
    case Paused(id, i, r)           => debug("Engine: Action paused") *>
      actionPause(id, i, r) *> pure(SystemUpdate(se, Outcome.Ok))
    case Failed(id, i, e)           => logError(e) *> fail(id)(i, e) *>
      pure(SystemUpdate(se, Outcome.Ok))
    case Busy(id, _)                => warning(s"Cannot run sequence ${id.format} " +
      s"because " +
      s"required systems are in use.") *> pure(SystemUpdate(se, Outcome.Ok))
    case BreakpointReached(_)       => debug("Engine: Breakpoint reached") *>
      pure(SystemUpdate(se, Outcome.Ok))
    case Executed(id)               => debug("Engine: Execution completed") *>
      next(id) *> pure(SystemUpdate(se, Outcome.Ok))
    case Executing(id)              => debug("Engine: Executing") *>
      execute(id) *> pure(SystemUpdate(se, Outcome.Ok))
    case Finished(id)               => debug("Engine: Finished") *>
      switch(id)(SequenceState.Completed) *> pure(SystemUpdate(se, Outcome.Ok))
    case SingleRunCompleted(c, r)   =>
      debug(s"Engine: single action $c completed with result $r") *>
        completeSingleRun(c, r.response) *> pure(SystemUpdate(se, Outcome.Ok))
    case SingleRunFailed(c, e)      =>
      debug(s"Engine: single action $c failed with error $e") *>
        failSingleRun(c, e) *> pure(SystemUpdate(se, Outcome.Ok))
    case Null                       => pure(SystemUpdate(se, Outcome.Ok))
  }

  /**
    * Main logical thread to handle events and produce output.
    */
  private def run(userReact: PartialFunction[SystemEvent[F], HandleType[Unit]])(ev: EventType)(implicit ci: Concurrent[F]): HandleType[ResultType] = {
    ev match {
      case EventUser(ue)   => handleUserEvent(ue)
      case EventSystem(se) => handleSystemEvent(se).flatMap(x =>
        userReact.applyOrElse(se, (_: SystemEvent[F]) => unit).as(x))
    }
  }

  /** Traverse a process with a stateful computation. */
  // input, stream of events
  // initalState: state
  // f takes an event and the current state, it produces a new state, a new value B and more actions
  def mapEvalState[A, B](input: Stream[F, A],
                            initialState: S, f: (A, S) => F[(S, B, Option[Stream[F, A]])])
                           (implicit ev: Concurrent[F]): Stream[F, B] = {
    Stream.eval(fs2.concurrent.Queue.unbounded[F, Stream[F, A]]).flatMap { q =>
      Stream.eval_(q.enqueue1(input)) ++
        q.dequeue.parJoinUnbounded.evalMapAccumulate(initialState) { (s, a) =>
          f(a, s).flatMap {
            case (ns, b, None)     => (ns, b).pure[F]
            case (ns, b, Some(st)) => q.enqueue1(st) >> (ns, b).pure[F]
          }
        }.map(_._2)
    }
  }

  private def runE(userReact: PartialFunction[SystemEvent[F], HandleType[Unit]])
                  (ev: EventType,s: S)
                  (implicit ci: Concurrent[F])
  : F[(S, (ResultType, S), Option[Stream[F, EventType]])] =
    run(userReact)(ev).run.run(s).map {
      case (si, (r, p)) => (si, (r, si), p)
    }

  def process(userReact: PartialFunction[SystemEvent[F], HandleType[Unit]])
             (input: Stream[F, EventType])(qs: S)(implicit ev: Concurrent[F])
  : Stream[F, (ResultType, S)] =
    mapEvalState[EventType, (ResultType, S)](input, qs, runE(userReact)(_, _))

  // Functions for type bureaucracy

  def pure[A](a: A): HandleType[A] = a.pure[HandleType]

  val unit: HandleType[Unit] =
    Handle.unit

  val get: Handle[F, S, Event[F, S, U], S] =
    Handle.get

  private def inspect[A](f: S => A): HandleType[A] =
    Handle.inspect(f)

  def modify(f: S => S): HandleType[Unit] =
    Handle.modify(f)

  private def getS(id: Observation.Id): HandleType[Option[Sequence.State[F]]] =
    get.map(stateL.sequenceStateIndex(id).getOption(_))

  private def getSs[A](id: Observation.Id)(f: Sequence.State[F] => A): HandleType[Option[A]] =
    inspect(stateL.sequenceStateIndex(id).getOption(_).map(f))

  private def modifyS(id: Observation.Id)(f: Sequence.State[F] => Sequence.State[F]): HandleType[Unit] =
    modify(stateL.sequenceStateIndex(id).modify(f))

  private def putS(id: Observation.Id)(s: Sequence.State[F]): HandleType[Unit] =
    modify(stateL.sequenceStateIndex(id).set(s))

  // For debugging
  def printSequenceState(id: Observation.Id): HandleType[Unit] =
    getSs(id)((qs: Sequence.State[F]) => StateT.liftF(L.debug(s"$qs"))).void

}

object Engine {

  trait State[F[_], D] {
    def sequenceStateIndex(sid: Observation.Id): Optional[D, Sequence.State[F]]
  }

  trait Types[S, E] {
    type StateType = S
    type EventData = E
  }

}
