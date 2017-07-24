package edu.gemini.seqexec

import edu.gemini.seqexec.engine.Event._
import edu.gemini.seqexec.engine.Result.{PartialVal, RetVal}
import edu.gemini.seqexec.model.Model.{CloudCover, Conditions, ImageQuality, SequenceState, SkyBackground, WaterVapor, Operator, Observer}
import java.util.logging.{Logger => JLogger}

import scalaz._
import Scalaz._
import scalaz.concurrent.Task
import scalaz.stream.{Process, Sink, merge}

package engine {
  case class HandleP[A](run: Handle[(A, Option[Process[Task, Event]])])
  object HandleP {
    def fromProcess(p: Process[Task, Event]): HandleP[Unit] = HandleP(Applicative[Handle].pure[(Unit, Option[Process[Task, Event]])](((), Some(p))))
  }
  case class ActionMetadata(conditions: Conditions, operator: Option[Operator], observer: Option[Observer])
}

package object engine {

  // Top level synonyms

  /**
    * This represents an actual real-world action to be done in the underlying
    * systems.
    */
  type Action = Kleisli[Task, ActionMetadata, Result]

  /**
    * An `Execution` is a group of `Action`s that need to be run in parallel
    * without interruption. A *sequential* `Execution` can be represented with
    * an `Execution` with a single `Action`.
    */
  type Actions = List[Action]

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

  implicit val handlePInstances = new Applicative[HandleP] with Monad[HandleP] {
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
    *
    * It also takes care of initiating the execution when transitioning to
    * `Running` `Status`.
    */
  def switch(id: Sequence.Id)(st: SequenceState): HandleP[Unit] =
    resources.flatMap(
      other => getS(id).flatMap {
        case Some(seq) =>
          if (st === SequenceState.Running)
            // No resources being used by other running sequences
            if (seq.toSequence.resources.intersect(other).isEmpty)
              putS(id)(Sequence.State.status.set(seq, st))
            // Some resources are being used
            else send(busy(id))
          else putS(id)(Sequence.State.status.set(seq, st))
        case None => unit
      }
    )

  val resources: HandleP[Set[Resource]] =
    gets(_.sequences
          .values
          .toList
          .filter(_.status === SequenceState.Running)
          .foldMap(_.toSequence.resources)
    )

  def rollback(id: Sequence.Id): HandleP[Unit] =
    modifyS(id)(_.rollback)

  def setOperator(name: String): HandleP[Unit] =
    modify(st => Engine.State(st.conditions, name.some, st.sequences))

  def setObserver(id: Sequence.Id)(name: String): HandleP[Unit] =
    modifyS(id)(_.setObserver(name))

  def setConditions(conditions: Conditions): HandleP[Unit] =
    modify(st => Engine.State(conditions, st.operator, st.sequences))

  def setImageQuality(iq: ImageQuality): HandleP[Unit] =
    modify(st => Engine.State(st.conditions.copy(iq = iq), st.operator, st.sequences))

  def setWaterVapor(wv: WaterVapor): HandleP[Unit] =
    modify(st => Engine.State(st.conditions.copy(wv = wv), st.operator, st.sequences))

  def setSkyBackground(sb: SkyBackground): HandleP[Unit] =
    modify(st => Engine.State(st.conditions.copy(sb = sb), st.operator, st.sequences))

  def setCloudCover(cc: CloudCover): HandleP[Unit] =
    modify(st => Engine.State(st.conditions.copy(cc = cc), st.operator, st.sequences))


  /**
    * Load a Sequence
    */
  def load(id: Sequence.Id, seq: Sequence[Action \/ Result]): HandleP[Unit] =
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
          case SequenceState.Stopping =>
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
  private def execute(id: Sequence.Id): HandleP[Unit] = {

    // Send the expected event when the `Action` is executed
    // It doesn't catch run time exceptions. If desired, the Action as to do it itself.
    def act(t: (Action, Int), cx: ActionMetadata): Process[Task, Event] = t match {
      case (action, i) =>
        Process.eval(action(cx)).flatMap {
          case r@Result.OK(_)         => Process(completed(id, i, r))
          case r@Result.Partial(_, c) => Process(partial(id, i, r)) ++ act((c, i), cx)
          case e@Result.Error(_)      => Process(failed(id, i, e))
        }
    }

    get.flatMap(st => st.sequences.get(id).map { seq =>
        seq match {
          case Sequence.State.Final(_, _) =>
            // The sequence is marked as completed here
            putS(id)(seq) *> send(finished(id))
          case _                          => {
            val u = seq.current.actions.zipWithIndex.map(x => act(x, ActionMetadata(st.conditions, st.operator, seq.toSequence.metadata.observer)))
            val v = merge.mergeN(Process.emitAll(u)) ++ Process(executed (id))
            HandleP.fromProcess(v)
          }
        }
      }.getOrElse(unit)
    )
  }

  private def getState(f: Engine.State => Task[Option[Process[Task, Event]]]): HandleP[Unit] =
    get.flatMap(s => HandleP(f(s).liftM[HandleStateT].map((Unit, _))))

  /**
    * Given the index of the completed `Action` in the current `Execution`, it
    * marks the `Action` as completed and returns the new updated `State`.
    *
    * When the index doesn't exist it does nothing.
    */
  def complete[R<:RetVal](id: Sequence.Id, i: Int, r: Result.OK[R]): HandleP[Unit] = modifyS(id)(_.mark(i)(r))

  def partialResult[R<:PartialVal](id: Sequence.Id, i: Int, p: Result.Partial[R]): HandleP[Unit] = modifyS(id)(_.mark(i)(p))

  /**
    * For now it only changes the `Status` to `Paused` and returns the new
    * `State`. In the future this function should handle the failed
    * action.
    */
  def fail(id: Sequence.Id)(i: Int, e: Result.Error): HandleP[Unit] =
    modifyS(id)(_.mark(i)(e)) *>
      switch(id)(SequenceState.Error("There was an error"))

  /**
    * Ask for the current Handle `Status`.
    */
  def status(id: Sequence.Id): HandleP[Option[SequenceState]] = gets(_.sequences.get(id).map(_.status))

  // You shouldn't need to import this but if you do you could use the qualified
  // import: `engine.Logger`
  object Logger {

    private val logger = JLogger.getLogger(getClass.getName)

    /**
      * Log info lifted into Handle.
      */
    def info(msg: String): HandleP[Unit] = pure((logger.info(msg), None))

    /**
      * Log warning lifted into Handle.
      */
    def warning(msg: String): HandleP[Unit] = pure((logger.warning(msg), None))
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
      case Start(id)               => Logger.info("Engine: Started") *> rollback(id) *> switch(id)(SequenceState.Running) *> send(Event.executing(id))
      case Pause(id)               => Logger.info("Engine: Paused") *> switch(id)(SequenceState.Stopping)
      case Load(id, seq)           => Logger.info("Engine: Sequence loaded") *> load(id, seq)
      case Unload(id)              => Logger.info("Engine: Sequence unloaded") *> unload(id)
      case Breakpoint(id, step, v) => Logger.info("Engine: breakpoint changed") *> modifyS(id)(_.setBreakpoint(step, v))
      case SetOperator(name)       => Logger.info("Engine: Setting Operator name") *> setOperator(name)
      case SetObserver(id, name)   => Logger.info("Engine: Setting Observer name") *> setObserver(id)(name)
      case SetConditions(conds)    => Logger.info("Engine: Setting conditions") *> setConditions(conds)
      case SetImageQuality(iq)     => Logger.info("Engine: Setting image quality") *> setImageQuality(iq)
      case SetWaterVapor(wv)       => Logger.info("Engine: Setting water vapor") *> setWaterVapor(wv)
      case SetSkyBackground(sb)    => Logger.info("Engine: Setting sky background") *> setSkyBackground(sb)
      case SetCloudCover(cc)       => Logger.info("Engine: Setting cloud cover") *> setCloudCover(cc)
      case Poll                    => Logger.info("Engine: Polling current state")
      case GetState(f)             => getState(f)
      case Log(msg)                => Logger.info(msg)
    }

    def handleSystemEvent(se: SystemEvent): HandleP[Unit] = se match {
      case Completed(id, i, r)     => Logger.info("Engine: Action completed") *> complete(id, i, r)
      case PartialResult(id, i, r) => Logger.info("Engine: Partial result") *> partialResult(id, i, r)
      case Failed(id, i, e)        => Logger.info("Engine: Action failed") *> fail(id)(i, e)
      case Busy(id)                => Logger.info("Engine: Resources needed for this sequence are in use")
      case Executed(id)            => Logger.info("Engine: Execution completed") *> next(id)
      case Executing(id)           => Logger.info("Engine: Executing") *> execute(id)
      case Finished(id)            => Logger.info("Engine: Finished") *> switch(id)(SequenceState.Completed)
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

  private val unit: HandleP[Unit] = pure(Unit)

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
  def printSequenceState(id: Sequence.Id): HandleP[Option[Unit]] = getSs(id)((qs: Sequence.State) => Task.now(println(qs)).liftM[HandleStateT])

  // The `Catchable` instance of `Handle`` needs to be manually written.
  // Without it it's not possible to use `Handle` as a scalaz-stream process effects.
  implicit val engineInstance: Catchable[Handle] =
    new Catchable[Handle] {
      def attempt[A](a: Handle[A]): Handle[Throwable \/ A] = a.flatMap(
        x => Catchable[Task].attempt(Applicative[Task].pure(x)).liftM[HandleStateT]
      )
      def fail[A](err: Throwable) = Catchable[Task].fail(err).liftM[HandleStateT]
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
