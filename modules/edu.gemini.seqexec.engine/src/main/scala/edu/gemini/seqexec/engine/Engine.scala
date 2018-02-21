// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.engine

import edu.gemini.seqexec.engine.Event._
import edu.gemini.seqexec.engine.Result.{PartialVal, PauseContext, RetVal}
import edu.gemini.seqexec.model.Model.{CloudCover, Conditions, ImageQuality, Observer, Operator, Resource, SequenceState, SkyBackground, WaterVapor}
import org.log4s.getLogger

import scalaz.concurrent.Task
import scalaz.{Applicative, Kleisli, MonadState, StateT}
import scalaz._
import Scalaz._
import scalaz.stream.{Process, merge}

class Engine[D] {
  /**
    * Changes the `Status` and returns the new `Queue.State`.
    */
  private def switch(id: Sequence.Id)(st: SequenceState): HandleP[Unit, D] =
    modifyS(id)(s => Sequence.State.status.set(st)(s))

  private def start(id: Sequence.Id): HandleP[Unit, D] =
    resources.flatMap(
      other => getS(id).flatMap {
        case Some(seq) =>
          // No resources being used by other running sequences
          if (seq.status.isIdle || seq.status.isError)
            if (seq.toSequence.resources.intersect(other).isEmpty)
              putS(id)(Sequence.State.status.set(SequenceState.Running.init)(seq.skips.getOrElse(seq).rollback)) *>
                send(Event.executing(id))
            // Some resources are being used
            else send(busy(id))
          else unit
        case None      => unit
      }
    )

  private def pause(id: Sequence.Id): HandleP[Unit, D] = modifyS(id)(Sequence.State.userStopSet(true))

  private def cancelPause(id: Sequence.Id): HandleP[Unit, D] = modifyS(id)(Sequence.State.userStopSet(false))

  private def resources: HandleP[Set[Resource], D] =
    gets(_.sequences
      .values
      .toList
      .filter(Sequence.State.isRunning)
      .foldMap(_.toSequence.resources)
    )

  //  private def rollback(id: Sequence.Id): HandleP[Unit, D] =
  //    modifyS(id)(_.rollback)

  private def setOperator(name: Operator): HandleP[Unit, D] =
    modify(_.copy(operator = name.some))

  private def setObserver(id: Sequence.Id)(name: Observer): HandleP[Unit, D] =
    modifyS(id)(_.setObserver(name))

  private def setConditions(conditions: Conditions): HandleP[Unit, D] =
    modify(_.copy(conditions = conditions))

  private def setImageQuality(iq: ImageQuality): HandleP[Unit, D] =
    modify(st => st.copy(conditions = st.conditions.copy(iq = iq)))

  private def setWaterVapor(wv: WaterVapor): HandleP[Unit, D] =
    modify(st => st.copy(conditions = st.conditions.copy(wv = wv)))

  private def setSkyBackground(sb: SkyBackground): HandleP[Unit, D] =
    modify(st => st.copy(conditions = st.conditions.copy(sb = sb)))

  private def setCloudCover(cc: CloudCover): HandleP[Unit, D] =
    modify(st => st.copy(conditions = st.conditions.copy(cc = cc)))

  /**
    * Load a Sequence
    */
  private def load(id: Sequence.Id, seq: Sequence): HandleP[Unit, D] =
    modify(
      st => Engine.State[D](
        st.userData,
        st.conditions,
        st.operator,
        st.sequences.get(id).map(t =>
          if (Sequence.State.isRunning(t)) st.sequences
          else st.sequences.updated(id, Sequence.State.init(seq))
        ).getOrElse(st.sequences.updated(id, Sequence.State.init(seq)))
      )
    )

  private def unload(id: Sequence.Id): HandleP[Unit, D] =
    modify(
      st => Engine.State[D](
        st.userData,
        st.conditions,
        st.operator,
        st.sequences.get(id).map(t =>
          if (Sequence.State.isRunning(t)) st.sequences
          else st.sequences - id
        ).getOrElse(st.sequences)
      )
    )

  /**
    * Adds the current `Execution` to the completed `Queue`, makes the next
    * pending `Execution` the current one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  private def next(id: Sequence.Id): HandleP[Unit, D] =
    getS(id).flatMap(
      _.map { seq =>
        if (Sequence.State.anyStopRequested(seq))
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
        else if (Sequence.State.isRunning(seq))
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
        else unit
      }.getOrElse(unit)
    )

  /**
    * Executes all actions in the `Current` `Execution` in parallel. When all are done it emits the `Executed` event.
    * It also updates the `State` as needed.
    */
  // Send the expected event when the `Action` is executed
  // It doesn't catch run time exceptions. If desired, the Action has to do it itself.
  private def act(id: Sequence.Id, t: (ActionGen, Int), cx: ActionMetadata): Process[Task, Event[D]] = t match {
    case (gen, i) =>
      Process.eval(gen(cx)).flatMap {
        case r@Result.OK(_)         => Process(completed(id, i, r))
        case r@Result.Partial(_, c) => Process(partial(id, i, r)) ++ act(id, (c, i), cx)
        case e@Result.Error(_)      => Process(failed(id, i, e))
        case r@Result.Paused(_)     => Process(paused(id, i, r))
      }
  }

  private def execute(id: Sequence.Id): HandleP[Unit, D] = {
    get.flatMap(st => st.sequences.get(id).map {
      case seq@Sequence.State.Final(_, _) =>
        // The sequence is marked as completed here
        putS(id)(seq) *> send(finished(id))
      case seq                            =>
        val u = seq.current.actions.map(_.gen).zipWithIndex.map(x => act(id, x, ActionMetadata(st.conditions, st.operator, seq.toSequence.metadata.observer)))
        val v = merge.mergeN(Process.emitAll(u))
        val w = seq.current.actions.indices.map(i => modifyS(id)(_.start(i))).toList
        w.sequenceU *> HandleP.fromProcess[D](v)
    }.getOrElse(unit)
    )
  }

  private def getState(f: Engine.State[D] => Task[Option[Process[Task, Event[D]]]]): HandleP[Unit, D] =
    get.flatMap(s => HandleP[Unit, D](f(s).liftM[HandleStateT[?[_], ?, D]].map(((), _))))

  private def getSeqState(id: Sequence.Id, f: Sequence.State => Option[Process[Task, Event[D]]]): HandleP[Unit, D] =
    getS(id).flatMap(_.map(s => HandleP[Unit, D](f(s).pure[Handle[?, D]].map(((), _)))).getOrElse(unit))

  private def actionStop(id: Sequence.Id, f: (Sequence.State) => Option[Process[Task, Event[D]]]): HandleP[Unit, D] =
    getS(id).flatMap(_.map(s => if (Sequence.State.isRunning(s)) HandleP[Unit, D](f(s).pure[Handle[?, D]].map(((), _))) *> modifyS(id)(Sequence.State.internalStopSet(true)) else unit).getOrElse(unit))

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exist it does nothing.
    */
  private def complete[R <: RetVal](id: Sequence.Id, i: Int, r: Result.OK[R]): HandleP[Unit, D] = modifyS(id)(_.mark(i)(r)) *>
    getS(id).flatMap(_.flatMap(
      _.current.execution.all(Action.completed).option(HandleP.fromProcess[D](Process(executed(id))))
    ).getOrElse(unit))

  private def partialResult[R <: PartialVal](id: Sequence.Id, i: Int, p: Result.Partial[R]): HandleP[Unit, D] = modifyS(id)(_.mark(i)(p))

  def actionPause[C <: PauseContext](id: Sequence.Id, i: Int, p: Result.Paused[C]): HandleP[Unit, D] = modifyS(id)(s => Sequence.State.internalStopSet(false)(s).mark(i)(p))

  private def actionResume(id: Sequence.Id, i: Int, cont: Task[Result]): HandleP[Unit, D] = getS(id).flatMap(_.map { s =>
    if (Sequence.State.isRunning(s) && s.current.execution.index(i).exists(Action.paused))
      modifyS(id)(_.start(i)) *> HandleP.fromProcess(act(id, (Kleisli(_ => cont), i), ActionMetadata.default))
    else unit
  }.getOrElse(unit))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  private def fail(id: Sequence.Id)(i: Int, e: Result.Error): HandleP[Unit, D] =
    modifyS(id)(_.mark(i)(e)) *>
      switch(id)(SequenceState.Failed(e.msg))

  private def logError(e: Result.Error): HandleP[Unit, D] = Logger.error(e.errMsg.getOrElse(e.msg))

  /**
    * Ask for the current Handle `Status`.
    */
  //  private def status(id: Sequence.Id): HandleP[Option[SequenceState], D] = gets(_.sequences.get(id).map(_.status))

  // You shouldn't need to import this but if you do you could use the qualified
  // import: `engine.Logger`
  private object Logger {

    private val logger = getLogger

    /**
      * Log info lifted into Handle.
      */
    def info(msg: => String): HandleP[Unit, D] = pure((logger.info(msg), None)).void

    /**
      * Log warning lifted into Handle.
      */
    def warning(msg: => String): HandleP[Unit, D] = pure((logger.warn(msg), None)).void

    /**
      * Log debug lifted into Handle.
      */
    def debug(msg: => String): HandleP[Unit, D] = pure((logger.debug(msg), None)).void

    /**
      * Log error lifted into Handle
      */
    def error(msg: => String): HandleP[Unit, D] = pure((logger.error(msg), None)).void

  }

  /**
    * Enqueue `Event` in the Handle.
    */
  private def send(ev: Event[D]): HandleP[Unit, D] = HandleP.fromProcess[D](Process(ev))

  /**
    * Main logical thread to handle events and produce output.
    */
  private def run(ev: Event[D]): HandleP[Engine.State[D], D] = {
    def handleUserEvent(ue: UserEvent[D]): HandleP[Unit, D] = ue match {
      case Start(id, _)               => Logger.debug("Engine: Started") *> start(id)
      case Pause(id, _)               => Logger.debug("Engine: Pause requested") *> pause(id)
      case CancelPause(id, _)         => Logger.debug("Engine: Pause canceled") *> cancelPause(id)
      case Load(id, seq)              => Logger.debug("Engine: Sequence loaded") *> load(id, seq)
      case Unload(id)                 => Logger.debug("Engine: Sequence unloaded") *> unload(id)
      case Breakpoint(id, _, step, v) => Logger.debug(s"Engine: breakpoint changed for step $step to $v") *>
        modifyS(id)(_.setBreakpoint(step, v))
      case SkipMark(id, _, step, v)   => Logger.debug(s"Engine: skip mark changed for step $step to $v") *>
        modifyS(id)(_.setSkipMark(step, v))
      case SetOperator(name, _)       => Logger.debug(s"Engine: Setting Operator name to '$name' by ${ue.username}") *> setOperator(name)
      case SetObserver(id, _, name)   => Logger.debug(s"Engine: Setting Observer for observation $id to '$name' by ${ue.username}") *> setObserver(id)(name)
      case SetConditions(conds, _)    => Logger.debug("Engine: Setting conditions") *> setConditions(conds)
      case SetImageQuality(iq, _)     => Logger.debug("Engine: Setting image quality") *> setImageQuality(iq)
      case SetWaterVapor(wv, _)       => Logger.debug("Engine: Setting water vapor") *> setWaterVapor(wv)
      case SetSkyBackground(sb, _)    => Logger.debug("Engine: Setting sky background") *> setSkyBackground(sb)
      case SetCloudCover(cc, _)       => Logger.debug("Engine: Setting cloud cover") *> setCloudCover(cc)
      case Poll                       => Logger.debug("Engine: Polling current state")
      case GetState(f)                => getState(f)
      case ModifyState(f)             => modify(f)
      case GetSeqState(id, f)         => getSeqState(id, f)
      case ActionStop(id, f)          => Logger.debug("Engine: Action stop requested") *> actionStop(id, f)
      case ActionResume(id, i, cont)  => Logger.debug("Engine: Action resume requested") *> actionResume(id, i, cont)
      case LogDebug(msg)              => Logger.debug(msg)
      case LogInfo(msg)               => Logger.info(msg)
      case LogWarning(msg)            => Logger.warning(msg)
      case LogError(msg)              => Logger.error(msg)
    }

    def handleSystemEvent(se: SystemEvent): HandleP[Unit, D] = se match {
      case Completed(id, i, r)     => Logger.debug("Engine: Action completed") *> complete(id, i, r)
      case PartialResult(id, i, r) => Logger.debug("Engine: Partial result") *> partialResult(id, i, r)
      case Paused(id, i, r)        => Logger.debug("Engine: Action paused") *> actionPause(id, i, r)
      case Failed(id, i, e)        => logError(e) *> fail(id)(i, e)
      case Busy(id)                => Logger.warning(s"Cannot run sequence $id because required systems are in use.")
      case BreakpointReached(_)    => Logger.debug("Engine: Breakpoint reached")
      case Executed(id)            => Logger.debug("Engine: Execution completed") *> next(id)
      case Executing(id)           => Logger.debug("Engine: Executing") *> execute(id)
      case Finished(id)            => Logger.debug("Engine: Finished") *> switch(id)(SequenceState.Completed)
      case Null                    => unit
    }

    (ev match {
      case EventUser(ue)   => handleUserEvent(ue)
      case EventSystem(se) => handleSystemEvent(se)
    }) *> get
  }

  // Kudos to @tpolecat
  /** Traverse a process with a stateful computation. */
  private def mapEvalState[A, S, B](
                                     fs: Process[Task, A], s0: S, f: A => StateT[Task, S, (B, Option[Process[Task, A]])]
                                   ): Process[Task, B] = {
    def go(fi: Process[Task, A], si: S): Process[Task, B] = {
      Process.eval(fi.unconsOption).flatMap {
        case None => Process.halt
        case Some((h, t)) => Process.eval(f(h).run(si)).flatMap {
          case (s, (b, p)) => Process.emit(b) ++ go(p.map(_ merge t).getOrElse(t), s)
        }
      }
    }

    go(fs, s0)
  }

  private def runE(ev: Event[D]): HandleP[(Event[D], Engine.State[D]), D] =
    run(ev).map((ev, _))

  def process(input: Process[Task, Event[D]])(qs: Engine.State[D]): Process[Task, (Event[D], Engine.State[D])] =
    mapEvalState[Event[D], Engine.State[D], (Event[D], Engine.State[D])](input, qs, (e: Event[D]) => runE(e).run)

  // Functions for type bureaucracy

  private def pure[A](a: A): HandleP[A, D] = Applicative[HandleP[?, D]].pure(a)

  private val unit: HandleP[Unit, D] = pure(())

  private val get: HandleP[Engine.State[D], D] =
    MonadState[Handle[?, D], Engine.State[D]].get.toHandleP

  private def gets[A](f: (Engine.State[D]) => A): HandleP[A, D] =
    MonadState[Handle[?, D], Engine.State[D]].gets(f).toHandleP

  private def modify(f: (Engine.State[D]) => Engine.State[D]): HandleP[Unit, D] =
    MonadState[Handle[?, D], Engine.State[D]].modify(f).toHandleP

  private def getS(id: Sequence.Id): HandleP[Option[Sequence.State], D] = get.map(_.sequences.get(id))

  private def getSs[A](id: Sequence.Id)(f: Sequence.State => A): HandleP[Option[A], D] =
    gets(_.sequences.get(id).map(f))

  private def modifyS(id: Sequence.Id)(f: Sequence.State => Sequence.State): HandleP[Unit, D] =
    modify(
      st => Engine.State(
        st.userData,
        st.conditions,
        st.operator,
        st.sequences.get(id).map(
          s => st.sequences.updated(id, f(s))).getOrElse(st.sequences)
      )
    )

  private def putS(id: Sequence.Id)(s: Sequence.State): HandleP[Unit, D] =
    modify(st => Engine.State[D](st.userData, st.conditions, st.operator, st.sequences.updated(id, s)))

  // For debugging
  def printSequenceState(id: Sequence.Id): HandleP[Unit, D] =
    getSs(id)((qs: Sequence.State) => Task.now(println(qs)).liftM[HandleStateT[?[_], ?, D]]).void // scalastyle:ignore

}

object Engine {

  final case class State[D](userData: D, conditions: Conditions, operator: Option[Operator], sequences: Map[Sequence.Id, Sequence.State])

  object State {

    def empty[D](userData: D): State[D] = State(userData, Conditions.default, None, Map.empty)

  }

}
