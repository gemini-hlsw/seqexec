package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

/**
 * Flag to indicate whether the global execution is `Running` or `Waiting`.
 */
sealed trait Status

object Status {
  case object Waiting   extends Status
  case object Completed extends Status
  case object Running   extends Status
}
/**
  * This is the top level state exposed by the `Engine`. This is what gets
  * generated whenever it needs to react to an `Event`.
  */
sealed trait QState {

  /**
    * Returns a new `State` where the next pending `Sequence` is been made the
    * current `Sequence` under execution and the previous current `Sequence` is
    * placed in the completed `Queue`.
    *
    * If the current `Sequence` has `Step`s not completed or there are no more
    * pending `Seqeunce`s it returns `None`.
    */
  val next: Option[QState]

  val status: Status

  val pending: List[Sequence[Action]]

  /**
    * Current Execution
    */
  val current: Current

  val done: List[Sequence[Result]]

  /**
    * Given an index of a current `Action` it replaces such `Action` with the
    * `Result` and returns the new modified `State`.
    *
    * If the index doesn't exist, the new `State` is returned unmodified.
    */
  def mark(i: Int)(r: Result): QState

  /**
    * Unzip `State`. This creates a single `Queue` with either completed `Sequence`s
    * or pending `Sequence`s.
    */
  val toQueue: Queue[Action \/ Result]
}

/**
  * Initial `State`. This doesn't have any `Sequence` under execution, there are
  * only pending `Step`s.
  *
  */
case class QStateI(queue: Queue[Action], status: Status) extends QState { self =>

  val next: Option[QState] =
    QueueZ.currentify(queue).map(QStateZ(_, status))

  val pending: List[Sequence[Action]] = queue.sequences

  val current: Current = Current.empty

  val done: List[Sequence[Result]] = Nil

  def mark(i: Int)(r: Result): QState = self

  val toQueue: Queue[Action \/ Result] = queue.map(_.left)
}

/**
  * This is the `State` in Zipper mode, which means is under execution.
  *
  */
case class QStateZ(zipper: QueueZ, status: Status) extends QState { self =>

  val next: Option[QState] = zipper.next match {
    // Last execution
    case None    => zipper.uncurrentify.map(QStateF(_, status))
    case Some(x) => Some(QStateZ(x, status))
  }

  /**
    * Current Execution
    */
  val current: Current =
    // Queue
    zipper
      // Sequence
      .focus
      // Step
      .focus
      // Execution
      .focus

  val pending: List[Sequence[Action]] = zipper.pending

  val done: List[Sequence[Result]] = zipper.done

  def mark(i: Int)(r: Result): QState = {

    val zipper: QStateZ @> QueueZ =
      Lens.lensu((qs, z) => qs.copy(zipper = z), _.zipper)

    val current: QStateZ @> Current = zipper >=> QueueZ.current

    current.mod(_.mark(i)(r), self)
  }

  val toQueue: Queue[Action \/ Result] =
    Queue(
      done.map(_.map(_.right)) ++
      List(zipper.focus.toSequence) ++
      pending.map(_.map(_.left))
    )
}

/**
  * Final `State`. This doesn't have any `Sequence` under execution, there are
  * only completed `Step`s.
  *
  */
case class QStateF(queue: Queue[Result], status: Status) extends QState { self =>

  val next: Option[QState] = None

  val current: Current = Current.empty

  val pending: List[Sequence[Action]] = Nil

  val done: List[Sequence[Result]] = queue.sequences

  def mark(i: Int)(r: Result): QState = self

  val toQueue: Queue[Action \/ Result] = queue.map(_.right)
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
    * Initialize a `State` passing a `Queue` of pending `Sequence`s.
    */
  // TODO: Make this function `apply`?
  def init(q: Queue[Action]): QState = QStateI(q, Status.Waiting)
}
