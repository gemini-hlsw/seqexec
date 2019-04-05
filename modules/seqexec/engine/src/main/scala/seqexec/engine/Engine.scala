// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats._
import cats.data.StateT
import cats.effect.{Concurrent, IO}
import cats.implicits._
import seqexec.engine.Event._
import seqexec.engine.Result.{PartialVal, PauseContext, RetVal}
import seqexec.model.{ClientId, SequenceState, StepId}
import fs2.Stream
import gem.Observation
import monocle.Optional
import mouse.boolean._
import org.log4s.getLogger

class Engine[D, U](stateL: Engine.State[D]) {

  class ConcreteTypes extends Engine.Types {
    override type StateType = D
    override type EventData = U
  }
  type EventType = Event[ConcreteTypes]
  type ResultType = EventResult[ConcreteTypes]
  type UserEventType = UserEvent[ConcreteTypes]
  type HandleType[A] = Handle[D, EventType, A]

  /**
    * Changes the `Status` and returns the new `Queue.State`.
    */
  private def switch(id: Observation.Id)(st: SequenceState): HandleType[Unit] =
    modifyS(id)(Sequence.State.status.set(st))

  def start(id: Observation.Id, clientId: ClientId, userCheck: D => Boolean): HandleType[Unit] =
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

  /*
   * startFrom starts a sequence from an arbitrary step. It does it by marking all previous steps to be skipped and then
   * modifying the state sequence as if it was run.
   * If the requested step is already run or marked to be skipped, the sequence will start from the next runnable step
   */
  def startFrom(id: Observation.Id, step: StepId): HandleType[Unit] =
    getS(id).flatMap {
      case Some(seq) if (seq.status.isIdle || seq.status.isError) && seq.toSequence.steps.exists(_.id === step) =>
        val steps = seq.toSequence.steps.takeWhile(_.id =!= step).mapFilter(p => Step.status(p).canRunFrom.option(p.id))
        val withSkips = steps.foldLeft[Sequence.State[IO]](seq){ case (s, i) => s.setSkipMark(i, true) }
        putS(id)(
          Sequence.State.status.set(SequenceState.Running.init)(withSkips.skips.getOrElse(withSkips).rollback)
        ) *> send(Event.executing(id))
      case _ => unit
    }

  def pause(id: Observation.Id): HandleType[Unit] = modifyS(id)(Sequence.State.userStopSet(true))

  private def cancelPause(id: Observation.Id): HandleType[Unit] = modifyS(id)(Sequence.State.userStopSet(false))

  /**
    * Builds the initial state of a sequence
    */
  def load(seq: Sequence[IO]): Sequence.State[IO] = Sequence.State.init(seq)

  /**
    * Redefines an existing sequence. Changes the step actions, removes steps, adds new steps.
    */
  def reload(seq: Sequence.State[IO], steps: List[Step[IO]]): Sequence.State[IO] =
    Sequence.State.reload(steps, seq)

  def startSingle(c: ActionCoords): HandleType[EventResult.Outcome] = get.flatMap { st =>
    val x = for {
      seq <- stateL.sequenceStateIndex(c.sid).getOption(st)
      if (seq.status.isIdle || seq.status.isError) && !seq.getSingleState(c.actCoords).active
      act <- seq.getSingleAction(c.actCoords)
    } yield act.gen

    x.map(p =>
      modifyS(c.sid)(_.startSingle(c.actCoords)) *>
      Handle.fromStream[D, EventType](
        p.map{
          case r@Result.OK(_)    => singleRunCompleted(c, r)
          case e@Result.Error(_) => singleRunFailed(c, e)
          case r                 =>
            singleRunFailed(c, Result.Error(s"Unhandled result for single run action: $r"))
        }
      ).as[EventResult.Outcome](EventResult.Ok)
    ).getOrElse(pure[EventResult.Outcome](EventResult.Failure))

  }

  private def completeSingleRun[V <: RetVal](c: ActionCoords, r: V): HandleType[Unit] =
    modifyS(c.sid)(_.completeSingle(c.actCoords, r))

  private def failSingleRun(c: ActionCoords, e: Result.Error): HandleType[Unit] = modifyS(c.sid)(
    _.failSingle(c.actCoords, e)
  )

  /**
    * Tells if a sequence can be safely removed
    */
  def canUnload(id: Observation.Id)(st: D): Boolean =
    stateL.sequenceStateIndex(id).getOption(st).forall(!Sequence.State.isRunning(_))

  /**
    * Refresh the steps executions of an existing sequence. Does not add nor remove steps.
    * @param id sequence identifier
    * @param steps List of new steps definitions
    * @return
    */
  def update(id: Observation.Id, steps: List[Step[IO]]): Endo[D] =
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
            case Some(qs: Sequence.State.Final[IO]) =>
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
            case Some(qs: Sequence.State.Final[IO]) =>
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
  private def act(id: Observation.Id, stepId: StepId, t: (Stream[IO, Result], Int))
  : Stream[IO, EventType] = t match {
    case (gen, i) =>
      gen.map {
        case r@Result.OK(_)        => completed(id, stepId, i, r)
        case r@Result.OKStopped(_) => stopCompleted(id, stepId, i, r)
        case r@Result.Partial(_)   => partial(id, stepId, i, r)
        case e@Result.Error(_)     => failed(id, i, e)
        case r@Result.Paused(_)    => paused(id, i, r)
      }
  }

  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  private def execute(id: Observation.Id)(implicit ev: Concurrent[IO]): HandleType[Unit] = {
    get.flatMap(st => stateL.sequenceStateIndex(id).getOption(st).map {
      case seq@Sequence.State.Final(_, _)  =>
        // The sequence is marked as completed here
        putS(id)(seq) *> send(finished(id))
      case seq@Sequence.State.Zipper(z, _, _) =>
        val stepId = z.focus.toStep.id
        val u: List[Stream[IO, EventType]] = seq.current.actions.map(_.gen).zipWithIndex.map(x =>
          act(id, stepId, x))
        val v: Stream[IO, EventType] = Stream.emits(u).parJoin(u.length)
        val w: List[HandleType[Unit]] = seq.current.actions.indices.map(i => modifyS(id)(_.start(i))).toList
        w.sequence *> Handle.fromStream(v)
    }.getOrElse(unit) )
  }

  private def getState(f: D => Option[Stream[IO, EventType]]): HandleType[Unit] =
    get.flatMap(s => Handle[D, EventType, Unit](f(s).pure[StateT[IO, D, ?]].map(((), _))))

  private def actionStop(id: Observation.Id, f: D => Option[Stream[IO, EventType]]): HandleType[Unit] =
    getS(id).flatMap(_.map(s => if (Sequence.State.isRunning(s)) Handle(StateT[IO, D, (Unit, Option[Stream[IO, EventType]])](st => IO((st, ((), f(st)))))) *> modifyS(id)(Sequence.State.internalStopSet(true)) else unit).getOrElse(unit))

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exist it does nothing.
    */
  private def complete[R <: RetVal](id: Observation.Id, i: Int, r: Result.OK[R]): HandleType[Unit] = modifyS(id)(_.mark(i)(r)) *>
    getS(id).flatMap(_.flatMap(
      _.current.execution.forall(Action.completed).option(Handle.fromStream[D, EventType](Stream(executed(id))))
    ).getOrElse(unit))

  private def stopComplete[R <: RetVal](id: Observation.Id, i: Int, r: Result.OKStopped[R]): HandleType[Unit] = modifyS(id)(_.mark(i)(r)) *>
    getS(id).flatMap(_.flatMap(
      _.current.execution.forall(Action.completed).option(Handle.fromStream[D, EventType](Stream(executed(id))))
    ).getOrElse(unit))

  private def partialResult[R <: PartialVal](id: Observation.Id, i: Int, p: Result.Partial[R]): HandleType[Unit] = modifyS(id)(_.mark(i)(p))

  def actionPause[C <: PauseContext](id: Observation.Id, i: Int, p: Result.Paused[C]): HandleType[Unit] = modifyS(id)(s => Sequence.State.internalStopSet(false)(s).mark(i)(p))

  private def actionResume(id: Observation.Id, i: Int, cont: Stream[IO, Result])
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

  private def logError(e: Result.Error): HandleType[Unit] = Logger.error(e.errMsg.getOrElse(e.msg))

  // You shouldn't need to import this but if you do you could use the qualified
  // import: `engine.Logger`
  private object Logger {

    private val logger = getLogger

    /**
      * Log info lifted into Handle.
      */
    def info(msg: => String): HandleType[Unit] = pure((logger.info(msg), None)).void

    /**
      * Log warning lifted into Handle.
      */
    def warning(msg: => String): HandleType[Unit] = pure((logger.warn(msg), None)).void

    /**
      * Log debug lifted into Handle.
      */
    def debug(msg: => String): HandleType[Unit] = pure((logger.debug(msg), None)).void

    /**
      * Log error lifted into Handle
      */
    def error(msg: => String): HandleType[Unit] = pure((logger.error(msg), None)).void

  }

  /**
    * Enqueue `Event` in the Handle.
    */
  private def send(ev: EventType): HandleType[Unit] = Handle.fromStream(Stream(ev))

  private def handleUserEvent(ue: UserEventType): HandleType[ResultType] = ue match {
    case Start(id, _, clid, userCheck) => Logger.debug(s"Engine: Start requested for sequence ${id.format}") *> start(id, clid, userCheck) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case Pause(id, _)                  => Logger.debug(s"Engine: Pause requested for sequence ${id.format}") *> pause(id) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case CancelPause(id, _)            => Logger.debug(s"Engine: Pause canceled for sequence ${id.format}") *> cancelPause(id) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case Breakpoint(id, _, step, v)    => Logger.debug(s"Engine: breakpoint changed for sequence ${id.format} and step $step to $v") *>
      modifyS(id)(_.setBreakpoint(step, v)) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case SkipMark(id, _, step, v)      => Logger.debug(s"Engine: skip mark changed for sequence ${id.format} and step $step to $v") *>
      modifyS(id)(_.setSkipMark(step, v)) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case Poll(_)                       => Logger.debug("Engine: Polling current state") *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case GetState(f)                   => getState(f) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case ModifyState(f)                => f.map(r => UserCommandResponse[ConcreteTypes](ue, EventResult.Ok, Some(r)))
    case ActionStop(id, f)             => Logger.debug("Engine: Action stop requested") *> actionStop(id, f) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case ActionResume(id, i, cont)     => Logger.debug("Engine: Action resume requested") *> actionResume(id, i, cont) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case LogDebug(msg)                 => Logger.debug(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case LogInfo(msg)                  => Logger.info(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case LogWarning(msg)               => Logger.warning(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
    case LogError(msg)                 => Logger.error(msg) *> pure(UserCommandResponse(ue, EventResult.Ok, None))
  }

  private def handleSystemEvent(se: SystemEvent)(implicit ci: Concurrent[IO]): HandleType[ResultType] = se match {
    case Completed(id, _, i, r)        => Logger.debug(
      s"Engine: From sequence ${id.format}: Action completed ($r)") *> complete(id, i, r) *>
      pure(SystemUpdate(se, EventResult.Ok))
    case StopCompleted(id, _, i, r)    => Logger.debug(
      s"Engine: From sequence ${id.format}: Action completed with stop ($r)") *> stopComplete(id, i, r) *>
      pure(SystemUpdate(se, EventResult.Ok))
    case PartialResult(id, _, i, r) => Logger.debug(
      s"Engine: From sequence ${id.format}: Partial result ($r)")  *> partialResult(id, i, r) *>
      pure(SystemUpdate(se, EventResult.Ok))
    case Paused(id, i, r)           => Logger.debug("Engine: Action paused") *>
      actionPause(id, i, r) *> pure(SystemUpdate(se, EventResult.Ok))
    case Failed(id, i, e)           => logError(e) *> fail(id)(i, e) *>
      pure(SystemUpdate(se, EventResult.Ok))
    case Busy(id, _)                => Logger.warning(s"Cannot run sequence ${id.format} " +
      s"because " +
      s"required systems are in use.") *> pure(SystemUpdate(se, EventResult.Ok))
    case BreakpointReached(_)       => Logger.debug("Engine: Breakpoint reached") *>
      pure(SystemUpdate(se, EventResult.Ok))
    case Executed(id)               => Logger.debug("Engine: Execution completed") *>
      next(id) *> pure(SystemUpdate(se, EventResult.Ok))
    case Executing(id)              => Logger.debug("Engine: Executing") *>
      execute(id) *> pure(SystemUpdate(se, EventResult.Ok))
    case Finished(id)               => Logger.debug("Engine: Finished") *>
      switch(id)(SequenceState.Completed) *> pure(SystemUpdate(se, EventResult.Ok))
    case SingleRunCompleted(c, r)   =>
      Logger.debug(s"Engine: single action $c completed with result $r") *>
        completeSingleRun(c, r.response) *> pure(SystemUpdate(se, EventResult.Ok))
    case SingleRunFailed(c, e)      =>
      Logger.debug(s"Engine: single action $c failed with error $e") *>
        failSingleRun(c, e) *> pure(SystemUpdate(se, EventResult.Ok))
    case Null                       => pure(SystemUpdate(se, EventResult.Ok))
  }

  /**
    * Main logical thread to handle events and produce output.
    */
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  private def run(userReact: PartialFunction[SystemEvent, HandleType[Unit]])(ev: EventType)(implicit ci: Concurrent[IO]): HandleType[ResultType] = {
    ev match {
      case EventUser(ue)   => handleUserEvent(ue)
      case EventSystem(se) => handleSystemEvent(se).flatMap(x =>
        userReact.applyOrElse(se, (_:SystemEvent) =>  unit).as(x))
    }
  }

  /** Traverse a process with a stateful computation. */
  // input, stream of events
  // initalState: state
  // f takes an event and the current state, it produces a new state, a new value B and more actions
  @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
  def mapEvalState[A, S, B](input: Stream[IO, A],
                            initialState: S, f: (A, S) => IO[(S, B, Option[Stream[IO, A]])])
                           (implicit ev: Concurrent[IO]): Stream[IO, B] = {
    Stream.eval(fs2.concurrent.Queue.unbounded[IO, Stream[IO, A]]).flatMap { q =>
      Stream.eval_(q.enqueue1(input)) ++
        q.dequeue.parJoinUnbounded.evalMapAccumulate(initialState) { (s, a) =>
          f(a, s).flatMap {
            case (ns, b, None)     => IO.pure((ns, b))
            case (ns, b, Some(st)) => q.enqueue1(st) >> IO.pure((ns, b))
          }
        }.map(_._2)
    }
  }

  @SuppressWarnings(Array("org.wartremover.warts.AnyVal", "org.wartremover.warts.ImplicitParameter"))
  private def runE(userReact: PartialFunction[SystemEvent, HandleType[Unit]])
                  (ev: EventType,s: D)
                  (implicit ci: Concurrent[IO])
  : IO[(D, (ResultType, D), Option[Stream[IO, EventType]])] =
    run(userReact)(ev).run.run(s).map {
      case (si, (r, p)) => (si, (r, si), p)
    }

  @SuppressWarnings(Array("org.wartremover.warts.AnyVal", "org.wartremover.warts.ImplicitParameter"))
  def process(userReact: PartialFunction[SystemEvent, HandleType[Unit]])
             (input: Stream[IO, EventType])(qs: D)(implicit ev: Concurrent[IO])
  : Stream[IO, (ResultType, D)] =
    mapEvalState[EventType, D, (ResultType, D)](input, qs, runE(userReact)(_, _))

  // Functions for type bureaucracy

  def pure[A](a: A): HandleType[A] = Applicative[HandleType].pure(a)

  val unit: HandleType[Unit] =
    Handle.unit

  val get: HandleType[D] =
    Handle.get

  private def inspect[A](f: D => A): HandleType[A] =
    Handle.inspect(f)

  def modify(f: D => D): HandleType[Unit] =
    Handle.modify(f)

  private def getS(id: Observation.Id): HandleType[Option[Sequence.State[IO]]] =
    get.map(stateL.sequenceStateIndex(id).getOption(_))

  private def getSs[A](id: Observation.Id)(f: Sequence.State[IO] => A): HandleType[Option[A]] =
    inspect(stateL.sequenceStateIndex(id).getOption(_).map(f))

  private def modifyS(id: Observation.Id)(f: Sequence.State[IO] => Sequence.State[IO]): HandleType[Unit] =
    modify(stateL.sequenceStateIndex(id).modify(f))

  private def putS(id: Observation.Id)(s: Sequence.State[IO]): HandleType[Unit] =
    modify(stateL.sequenceStateIndex(id).set(s))

  // For debugging
  def printSequenceState(id: Observation.Id): HandleType[Unit] =
    getSs(id)((qs: Sequence.State[IO]) => StateT.liftF(IO.pure(println(qs)))).void // scalastyle:ignore

}

object Engine {

  trait State[D] {
    def sequenceStateIndex(sid: Observation.Id): Optional[D, Sequence.State[IO]]
  }

  abstract class Types {
    type StateType
    type EventData
  }

}
