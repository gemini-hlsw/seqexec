// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Event._
import edu.gemini.seqexec.engine.Result.{Error, PartialVal, PauseContext, RetVal}
import edu.gemini.seqexec.model.Model.{CloudCover, Conditions, ImageQuality, Observer, Operator, SequenceState, SkyBackground, WaterVapor}
import edu.gemini.seqexec.model.Model.Resource
import edu.gemini.seqexec.model.ActionType
import org.log4s.getLogger

import scalaz.Monad
import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.{Process, Sink, merge}

package engine {

  final case class HandleP[A](run: Handle[(A, Option[Process[Task, Event]])])
  object HandleP {
    def fromProcess(p: Process[Task, Event]): HandleP[Unit] = HandleP(Applicative[Handle].pure[(Unit, Option[Process[Task, Event]])](((), Some(p))))
  }
  final case class ActionMetadata(conditions: Conditions, operator: Option[Operator], observer: Option[Observer])
  object ActionMetadata {
    val default: ActionMetadata = ActionMetadata(Conditions.default, None, None)
  }

  final case class Action(
    kind: ActionType,
    gen: ActionGen,
    state: Action.State
  )
  object Action {

    final case class State(runState: ActionState, partials: List[PartialVal])

    sealed trait ActionState

    object ActionState {
      implicit val equal: Equal[ActionState] = Equal.equalA
    }

    object Idle extends ActionState

    object Started extends ActionState

    final case class Paused[C <: PauseContext](ctx: C) extends ActionState

    final case class Completed[V <: RetVal](r: V) extends ActionState

    final case class Failed(e: Error) extends ActionState

    def errored(ar: Action): Boolean = ar.state.runState match {
      case Action.Failed(_) => true
      case _                => false
    }

    def finished(ar: Action): Boolean = ar.state.runState match {
      case Action.Failed(_)    => true
      case Action.Completed(_) => true
      case _                   => false
    }

    def completed(ar: Action): Boolean = ar.state.runState match {
      case Action.Completed(_) => true
      case _                   => false
    }

    def paused(ar: Action): Boolean = ar.state.runState match {
      case Action.Paused(_) => true
      case _                => false
    }
  }

}

package object engine {

  // Top level synonyms

  /**
    * This represents an actual real-world action to be done in the underlying
    * systems.
    */
  def fromTask(kind: ActionType, t: Task[Result]): Action = Action(kind, Kleisli[Task, ActionMetadata, Result](_ => t), Action.State(Action.Idle, Nil))
  /**
    * An `Execution` is a group of `Action`s that need to be run in parallel
    * without interruption. A *sequential* `Execution` can be represented with
    * an `Execution` with a single `Action`.
    */
  type Actions = List[Action]

  type ActionGen = Kleisli[Task, ActionMetadata, Result]

  type Results = List[Result]

  type FileId = String

  // Handle proper

  /**
    * Type constructor where all Seqexec side effect are managed.
    *
    * It's named `Handle` after `fs2.Handle` in order to give a hint in a future
    * migration.
    */
  type Handle[A] = HandleStateT[Task, A]
  // Helper alias to facilitate lifting.
  type HandleStateT[M[_], A] = StateT[M, Engine.State, A]

  implicit val handlePInstances: Applicative[HandleP] with Monad[HandleP] = new Applicative[HandleP] with Monad[HandleP] {
    private def concatOpP(op1: Option[Process[Task, Event]],
                          op2: Option[Process[Task, Event]]): Option[Process[Task, Event]] = (op1, op2) match {
      case (None, None)         => None
      case (Some(p1), None)     => Some(p1)
      case (None, Some(p2))     => Some(p2)
      case (Some(p1), Some(p2)) => Some(p1 ++ p2)
    }

    override def point[A](a: => A): HandleP[A] = HandleP(Applicative[Handle].pure((a, None)))


    // I tried to use a for comprehension here, but the compiler failed with error
    // "value filter is not a member of edu.gemini.seqexec.engine.Handle"
    override def ap[A, B](fa: => HandleP[A])(f: => HandleP[(A) => B]): HandleP[B] = HandleP(
      f.run.flatMap{
        case (g, op2) => fa.run.map {
          case (a, op1) => (g(a), concatOpP(op1, op2)) } })

    override def bind[A, B](fa: HandleP[A])(f: (A) => HandleP[B]): HandleP[B] = HandleP(
      fa.run.flatMap{
        case (a, op1) => f(a).run.map{
          case (b, op2) => (b, concatOpP(op1, op2))
        }
      }
    )

  }

  implicit class HandleToHandleP[A](self: Handle[A]) {
    def toHandleP: HandleP[A] = HandleP(self.map((_, None)))
  }

  /**
    * Changes the `Status` and returns the new `Queue.State`.
    */
  def switch(id: Sequence.Id)(st: SequenceState): HandleP[Unit] =
    modifyS(id)(s => Sequence.State.status.set(st)(s))

  def start(id: Sequence.Id): HandleP[Unit] =
    resources.flatMap(
      other => getS(id).flatMap {
        case Some(seq) =>
          // No resources being used by other running sequences
          if (seq.status === SequenceState.Idle || SequenceState.isError(seq.status))
            if(seq.toSequence.resources.intersect(other).isEmpty)
              putS(id)(Sequence.State.status.set(SequenceState.Running)(seq.rollback)) *> send(Event.executing(id))
          // Some resources are being used
            else send(busy(id))
          else unit
        case None => unit
      }
    )

  def pause(id: Sequence.Id): HandleP[Unit] = modifyS(id)( s => if(s.status === SequenceState.Running) Sequence.State.status.set(SequenceState.Pausing)(s) else s)

  def cancelPause(id: Sequence.Id): HandleP[Unit] = modifyS(id)( s => if(s.status === SequenceState.Pausing) Sequence.State.status.set(SequenceState.Running)(s) else s)

  val resources: HandleP[Set[Resource]] =
    gets(_.sequences
          .values
          .toList
          .filter(_.status === SequenceState.Running)
          .foldMap(_.toSequence.resources)
    )

  def rollback(id: Sequence.Id): HandleP[Unit] =
    modifyS(id)(_.rollback)

  def setOperator(name: Operator): HandleP[Unit] =
    modify(_.copy(operator = name.some))
  def setObserver(id: Sequence.Id)(name: Observer): HandleP[Unit] =
    modifyS(id)(_.setObserver(name))

  def setConditions(conditions: Conditions): HandleP[Unit] =
    modify(_.copy(conditions = conditions))

  def setImageQuality(iq: ImageQuality): HandleP[Unit] =
    modify(st => st.copy(conditions = st.conditions.copy(iq = iq)))

  def setWaterVapor(wv: WaterVapor): HandleP[Unit] =
    modify(st => st.copy(conditions = st.conditions.copy(wv = wv)))

  def setSkyBackground(sb: SkyBackground): HandleP[Unit] =
    modify(st => st.copy(conditions = st.conditions.copy(sb = sb)))

  def setCloudCover(cc: CloudCover): HandleP[Unit] =
    modify(st => st.copy(conditions = st.conditions.copy(cc = cc)))

  /**
    * Load a Sequence
    */
  def load(id: Sequence.Id, seq: Sequence): HandleP[Unit] =
    modify(
      st => Engine.State(
        st.conditions,
        st.operator,
        st.sequences.get(id).map(
          t => t.status match {
            case SequenceState.Running => st.sequences
            case _                     => st.sequences.updated(id, Sequence.State.init(seq))
          }
        ).getOrElse(
          st.sequences.updated(id, Sequence.State.init(seq))
        )
      )
    )

  def unload(id: Sequence.Id): HandleP[Unit] =
    modify(
      st => Engine.State(
        st.conditions,
        st.operator,
        st.sequences.get(id).map(
          t => t.status match {
            case SequenceState.Running => st.sequences
            case _                     => st.sequences - id
          }
        ).getOrElse(st.sequences)
      )
    )

  /**
    * Adds the current `Execution` to the completed `Queue`, makes the next
    * pending `Execution` the current one, and initiates the actual execution.
    *
    * If there are no more pending `Execution`s, it emits the `Finished` event.
    */
  def next(id: Sequence.Id): HandleP[Unit] =
    getS(id).flatMap(
      _.map { seq =>
        seq.status match {
          case SequenceState.Pausing | SequenceState.Stopping =>
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
          case SequenceState.Running =>
            seq.next match {
              // Empty state
              case None =>
                send(finished(id))
              // Final State
              case Some(qs: Sequence.State.Final) =>
                putS(id)(qs) *> send(finished(id))
              // Execution completed. Check breakpoint here
              case Some(qs) =>
                putS(id)(qs) *> (if(qs.getCurrentBreakpoint) switch(id)(SequenceState.Idle)
                                 else send(executing(id)))
            }
          case _ => unit
        }
    }.getOrElse(unit)
  )

  /**
    * Executes all actions in the `Current` `Execution` in parallel. When all are done it emits the `Executed` event.
    * It also updates the `State` as needed.
    */
  // Send the expected event when the `Action` is executed
  // It doesn't catch run time exceptions. If desired, the Action has to do it itself.
  private def act(id: Sequence.Id, t: (ActionGen, Int), cx: ActionMetadata): Process[Task, Event] = t match {
    case (gen, i) =>
      Process.eval(gen(cx)).flatMap {
        case r@Result.OK(_)         => Process(completed(id, i, r))
        case r@Result.Partial(_, c) => Process(partial(id, i, r)) ++ act(id, (c, i), cx)
        case e@Result.Error(_)      => Process(failed(id, i, e))
        case r@Result.Paused(_)     => Process(paused(id, i, r))
      }
  }

  private def execute(id: Sequence.Id): HandleP[Unit] = {
    get.flatMap(st => st.sequences.get(id).map {
      case seq@Sequence.State.Final(_, _) =>
        // The sequence is marked as completed here
        putS(id)(seq) *> send(finished(id))
      case seq =>
        val u = seq.current.actions.map(_.gen).zipWithIndex.map(x => act(id, x, ActionMetadata(st.conditions, st.operator, seq.toSequence.metadata.observer)))
        val v = merge.mergeN(Process.emitAll(u))
        val w = seq.current.actions.indices.map(i => modifyS(id)(_.start(i))).toList
        w.sequenceU *> HandleP.fromProcess(v)
    }.getOrElse(unit)
    )
  }

  private def getState(f: Engine.State => Task[Option[Process[Task, Event]]]): HandleP[Unit] =
    get.flatMap(s => HandleP[Unit](f(s).liftM[HandleStateT].map(((), _))))

  private def getSeqState(id: Sequence.Id, f: Sequence.State => Option[Process[Task, Event]]): HandleP[Unit] =
    getS(id).flatMap(_.map(s => HandleP[Unit](f(s).pure[Handle].map(((), _)))).getOrElse(unit))

  private def actionStop(id: Sequence.Id, f: (Sequence.State) => Option[Process[Task, Event]]): HandleP[Unit] =
    getS(id).flatMap(_.map(s => if(s.status === SequenceState.Running) HandleP[Unit](f(s).pure[Handle].map(((), _))) *> switch(id)(SequenceState.Stopping) else unit).getOrElse(unit))

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exist it does nothing.
    */
  def complete[R<:RetVal](id: Sequence.Id, i: Int, r: Result.OK[R]): HandleP[Unit] = modifyS(id)(_.mark(i)(r)) *>
    getS(id).flatMap(_.flatMap{
      _.current.execution.all(Action.completed).option(HandleP.fromProcess(Process(executed(id))))
    }.getOrElse(unit))

  def partialResult[R<:PartialVal](id: Sequence.Id, i: Int, p: Result.Partial[R]): HandleP[Unit] = modifyS(id)(_.mark(i)(p))

  def actionPause[C<:PauseContext](id: Sequence.Id, i: Int, p: Result.Paused[C]): HandleP[Unit] = modifyS(id)(s => Sequence.State.status.set(SequenceState.Running)(s).mark(i)(p))

  def actionResume(id: Sequence.Id, i: Int, cont: Task[Result]): HandleP[Unit] = getS(id).flatMap(_.map{ s =>
    if (s.status === SequenceState.Running && s.current.execution.index(i).exists(Action.paused))
      modifyS(id)(_.start(i)) *> HandleP.fromProcess(act(id, (Kleisli(_ => cont), i), ActionMetadata.default))
    else unit
  }.getOrElse(unit))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  def fail(id: Sequence.Id)(i: Int, e: Result.Error): HandleP[Unit] =
    modifyS(id)(_.mark(i)(e)) *>
      switch(id)(SequenceState.Error(e.msg))

  /**
    * Ask for the current Handle `Status`.
    */
  def status(id: Sequence.Id): HandleP[Option[SequenceState]] = gets(_.sequences.get(id).map(_.status))

  // You shouldn't need to import this but if you do you could use the qualified
  // import: `engine.Logger`
  object Logger {

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
  }

  /**
    * Enqueue `Event` in the Handle.
    */
  private def send(ev: Event): HandleP[Unit] = HandleP.fromProcess(Process(ev))

  /**
    * Main logical thread to handle events and produce output.
    */
  private def run(ev: Event): HandleP[Engine.State] = {
    def handleUserEvent(ue: UserEvent): HandleP[Unit] = ue match {
      case Start(id, _)                => Logger.debug("Engine: Started") *> start(id)
      case Pause(id, _)                => Logger.debug("Engine: Pause requested") *> pause(id)
      case CancelPause(id, _)          => Logger.debug("Engine: Pause canceled") *> cancelPause(id)
      case Load(id, seq)               => Logger.debug("Engine: Sequence loaded") *> load(id, seq)
      case Unload(id)                  => Logger.debug("Engine: Sequence unloaded") *> unload(id)
      case Breakpoint(id, _, step, v)  => Logger.debug("Engine: breakpoint changed") *> modifyS(id)(_.setBreakpoint(step, v))
      case SetOperator(name, user)     => Logger.debug(s"Engine: Setting Operator name to '$name' by ${ue.username}") *> setOperator(name)
      case SetObserver(id, user, name) => Logger.debug(s"Engine: Setting Observer for observation $id to '$name' by ${ue.username}") *> setObserver(id)(name)
      case SetConditions(conds, user)  => Logger.debug("Engine: Setting conditions") *> setConditions(conds)
      case SetImageQuality(iq, user)   => Logger.debug("Engine: Setting image quality") *> setImageQuality(iq)
      case SetWaterVapor(wv, user)     => Logger.debug("Engine: Setting water vapor") *> setWaterVapor(wv)
      case SetSkyBackground(sb, user)  => Logger.debug("Engine: Setting sky background") *> setSkyBackground(sb)
      case SetCloudCover(cc, user)     => Logger.debug("Engine: Setting cloud cover") *> setCloudCover(cc)
      case Poll                        => Logger.debug("Engine: Polling current state")
      case GetState(f)                 => getState(f)
      case GetSeqState(id, f)          => getSeqState(id, f)
      case ActionStop(id, f)           => Logger.debug("Engine: Action stop requested") *> actionStop(id, f)
      case ActionResume(id, i, cont)   => Logger.debug("Engine: Action resume requested") *> actionResume(id, i, cont)
      case Log(msg)                    => Logger.debug(msg)
    }

    def handleSystemEvent(se: SystemEvent): HandleP[Unit] = se match {
      case Completed(id, i, r)     => Logger.debug("Engine: Action completed") *> complete(id, i, r)
      case PartialResult(id, i, r) => Logger.debug("Engine: Partial result") *> partialResult(id, i, r)
      case Paused(id, i, r)           => Logger.debug("Engine: Action paused")  *> actionPause(id, i, r)
      case Failed(id, i, e)        => Logger.debug("Engine: Action failed") *> fail(id)(i, e)
      case Busy(id)                => Logger.debug("Engine: Resources needed for this sequence are in use")
      case Executed(id)            => Logger.debug("Engine: Execution completed") *> next(id)
      case Executing(id)           => Logger.debug("Engine: Executing") *> execute(id)
      case Finished(id)            => Logger.debug("Engine: Finished") *> switch(id)(SequenceState.Completed)
      case Null                    => unit
    }

    (ev match {
        case EventUser(ue)   => handleUserEvent(ue)
        case EventSystem(se) => handleSystemEvent(se)
    })*> get
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

  def runE(ev: Event): HandleP[(Event, Engine.State)] =
    run(ev).map((ev, _))

  def process(input: Process[Task, Event])(qs: Engine.State): Process[Task, (Event, Engine.State)] =
    mapEvalState(input, qs, (e: Event) => runE(e).run)

  // Functions for type bureaucracy

  def pure[A](a: A): HandleP[A] = Applicative[HandleP].pure(a)

  private val unit: HandleP[Unit] = pure(())

  val get: HandleP[Engine.State] =
    MonadState[Handle, Engine.State].get.toHandleP

  private def gets[A](f: (Engine.State) => A): HandleP[A] =
    MonadState[Handle, Engine.State].gets(f).toHandleP

  private def modify(f: (Engine.State) => Engine.State): HandleP[Unit] =
    MonadState[Handle, Engine.State].modify(f).toHandleP

  private def getS(id: Sequence.Id): HandleP[Option[Sequence.State]] = get.map(_.sequences.get(id))

  private def getSs[A](id: Sequence.Id)(f: Sequence.State => A): HandleP[Option[A]] =
    gets(_.sequences.get(id).map(f))

  private def modifyS(id: Sequence.Id)(f: Sequence.State => Sequence.State): HandleP[Unit] =
    modify(
      st => Engine.State(
        st.conditions,
        st.operator,
        st.sequences.get(id).map(
          s => st.sequences.updated(id, f(s))).getOrElse(st.sequences)
      )
    )

  private def putS(id: Sequence.Id)(s: Sequence.State): HandleP[Unit] =
    modify(st => Engine.State(st.conditions, st.operator, st.sequences.updated(id, s)))

  // For debugging
  def printSequenceState(id: Sequence.Id): HandleP[Unit] =
    getSs(id)((qs: Sequence.State) => Task.now(println(qs)).liftM[HandleStateT]).void // scalastyle:ignore

  // The `Catchable` instance of `Handle`` needs to be manually written.
  // Without it it's not possible to use `Handle` as a scalaz-stream process effects.
  implicit val engineInstance: Catchable[Handle] =
    new Catchable[Handle] {
      def attempt[A](a: Handle[A]): Handle[Throwable \/ A] = a.flatMap(
        x => Catchable[Task].attempt(Applicative[Task].pure(x)).liftM[HandleStateT]
      )
      def fail[A](err: Throwable): HandleStateT[Task, A] = Catchable[Task].fail(err).liftM[HandleStateT]
    }

  /**
    * Lifts from `Task` to `Handle` as the effect of a `Process`.
    */
  def hoistHandle[A](p: Process[Task, A]): Process[Handle, A] = {
    val toHandle = new (Task ~> Handle) {
      def apply[B](t: Task[B]): Handle[B] = t.liftM[HandleStateT]
    }
    p.translate(toHandle)
  }

  /**
    * Lifts from `Task` to `Handle` as the effect of a `Sink`.
    */
  def hoistHandleSink[O](s: Sink[Task, O]): Sink[Handle, O] =
    hoistHandle(s).map(_.map(_.liftM[HandleStateT]))

}
