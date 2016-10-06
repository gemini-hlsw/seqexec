package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
 * Flag to indicate whether the global execution is `Running` or `Waiting`.
 */
sealed trait Status

object Status {
  case object Running extends Status
  case object Waiting extends Status
}

/**
  * This structure holds the `Execution` currently under execution. It carries
  * information about which `Action`s have been completed and the position in
  * the `Queue` for proper insertion into the completed `Queue` when all the
  * `Execution`s are done.
  */
case class Current(ars: List[Action \/ Result]) {

  def isEmpty: Boolean = ars.isEmpty

  def actions: List[Action] = {

    def lefts[L, R](xs: List[L \/ R]): List[L] = xs.collect { case -\/(l) => l }

    lefts(ars.toList)

  }

  def results: List[Result] = {

    def rights[L, R](xs: List[L \/ R]): List[R] = xs.collect { case \/-(r) => r }

    rights(ars.toList)

  }

  val uncurrentify: Option[Execution[Result]] =
    if (ars.all(_.isRight)) Some(results)
    else None

  /**
    * Set the `Result` for the given `Action` index in `Current`.
    *
    * If the index doesn't exist, `Current` is returned unmodified.
    */
  def mark(i: Int)(r: Result): Current =
    Current(PLens.listNthPLens(i).setOr(ars, r.right, ars))

}

object Current {

  val empty: Current = Current(Nil)

  def currentify(exe: Execution[Action]): Option[Current] =
    if (!exe.isEmpty) Some(Current(exe.map(_.left)))
    else None

}

/**
  * A list of `Executions` grouped by observation.
  */
case class Step[A](id: Int, executions: List[Execution[A]]) {

  def isEmpty: Boolean = executions.isEmpty

  /**
    * Adds a `Execution` to the front of a `Step`.
    */
  def cons(exe: Execution[A]): Step[A] = Step(id, exe :: executions)

  /**
    * Return the next `Execution` and the remaining `Step` if there are more
    * `Execution`s left.
    */
  def uncons: Option[(Execution[A], Step[A])] =
    executions.headOption.map((_, Step(id, executions.tail)))

}

object Step {

  def empty[A](id: Int): Step[A] = Step(id, Nil)

  def executions[A]: Step[A] @> List[Execution[A]] =
    Lens.lensu((s, exes) => s.copy(executions = exes), _.executions)

  implicit val stepFunctor = new Functor[Step] {
    def map[A, B](fa: Step[A])(f: A => B): Step[B] =
      Step(fa.id, fa.executions.map(_.map(f)))
  }

}

/**
  * A list of Steps grouped by target and instrument.
  */
case class Sequence[A](id: String, steps: List[Step[A]]) {

  def isEmpty: Boolean = steps.isEmpty

  def cons(step: Step[A]): Sequence[A] = Sequence(id, step :: steps)

  def uncons: Option[(Step[A], Sequence[A])] =
    steps.headOption.map((_, Sequence(id, steps.tail)))

}

object Sequence {

  def empty[A](id: String): Sequence[A] = Sequence(id, Nil)

  def steps[A]: Sequence[A] @> List[Step[A]] =
    Lens.lensu((s, sts) => s.copy(steps = sts), _.steps)

  implicit val SequenceFunctor = new Functor[Sequence] {
    def map[A, B](fa: Sequence[A])(f: A => B): Sequence[B] =
      Sequence(fa.id, fa.steps.map(_.map(f)))
  }

}

/**
  * A list of Sequences. The `Queue` could be empty of Sequences when waiting
  * for the addition of new ones.
  */
case class Queue[A](sequences: List[Sequence[A]]) {

  def isEmpty: Boolean = sequences.isEmpty

  def cons(seq: Sequence[A]): Queue[A] = Queue(seq :: sequences)

  def uncons: Option[(Sequence[A], Queue[A])] =
    sequences.headOption.map((_, Queue(sequences.tail)))

}

object Queue {

  def sequences[A]: Queue[A] @> List[Sequence[A]] =
    Lens.lensu((q, s) => q.copy(sequences = s), _.sequences)

  implicit def queueMonoid[A]: Monoid[Queue[A]] = new Monoid[Queue[A]] {
    def append(a: Queue[A], b: => Queue[A]): Queue[A] =
      Queue(a.sequences ::: b.sequences)

    val zero: Queue[A] = Queue(Nil)
  }

  implicit val queueFunctor = new Functor[Queue] {
    def map[A, B](fa: Queue[A])(f: A => B): Queue[B] =
      Queue(fa.sequences.map(_.map(f)))
  }

}

/**
  * This is the main state data type to be used by the `Engine`. This is what
  * gets modified whenever it needs to react to an Event.
  */
sealed trait QState {

  /**
    * Returns a new `State` where the next pending `Execution` has been promoted
    * to `Current` and `Current` is placed in the completed `Queue`. As a
    * convenience it also returns the `Execution` with pending `Actions` just
    * before making it `Current`.
    *
    * If the `Current` doesn't have all actions completed or there are no more
    * pending `Execution`s it returns None.
    */
  val next: Option[QState]

  val status: Status

  val pending: Queue[Action]

  val current: Current

  val done: Queue[Result]

  /**
    * Given an index of a current `Action` it replaces such `Action` with the
    * `Result` and returns the new modified `State`.
    *
    * If after marking the `Action`, all elements in `Current` are `Result`, it
    * empties `Current` and moves the `Result` to the completed `Queue`
    *
    * If the index doesn't exist, the new `State` is returned unmodified.
    */
  def mark(i: Int)(r: Result): QState

}

case class QStateI(pending: Queue[Action], status: Status) extends QState { self =>

  val next: Option[QState] =
    Zipper.QueueZ.currentify(pending).map(QStateZ(_, status))

  val current: Current = Current.empty

  val done: Queue[Result] = Queue(Nil)

  def mark(i: Int)(r: Result): QState = self

}

case class QStateZ(zipper: Zipper.QueueZ, status: Status) extends QState { self =>

  val next: Option[QState] = zipper.next match {
    // Last execution
    case None => zipper.uncurrentify.map(QStateF(_, status))
    case Some(x) => Some(QStateZ(x, status))
  }

  val current: Current = zipper.focus.focus.focus

  val pending: Queue[Action] = zipper.pending

  val done: Queue[Result] = zipper.done

  def mark(i: Int)(r: Result): QState = {

    val zipper: QStateZ @> Zipper.QueueZ =
      Lens.lensu((qs, z) => qs.copy(zipper = z), _.zipper)

    val current: QStateZ @> Current = zipper >=> Zipper.QueueZ.current

    current.mod(_.mark(i)(r), self)

  }

}

case class QStateF(done: Queue[Result], status: Status) extends QState { self =>

  val next: Option[QState] = None

  val current: Current = Current.empty

  val pending: Queue[Action] = Queue(Nil)

  def mark(i: Int)(r: Result): QState = self

}

object QState {

  val status: QState @> Status =
    // `QState` doesn't provide `.copy`
    Lens.lensu(
      (qs, s) => (
        qs match {
          // TODO: Isn't there a better way to write this?
          case QStateI(st, _) => QStateI(st, s)
          case QStateZ(st, _) => QStateZ(st, s)
          case QStateF(st, _) => QStateF(st, s)
        }
      ),
      _.status
    )

  /**
    * Initialize a `QStateZ` passing a `Queue` of `Action`s. This also takes care
    * of making the first pending `Execution` `Current`.
    */
  // TODO: Make this function `apply`?
  def init(q: Queue[Action]): QState = QStateI(q, Status.Waiting)

}

/**
  * The result of an `Action`.
  */
sealed trait Result

object Result {
  case class OK[R](r: R) extends Result
  case class Error[E](e: E) extends Result
}
