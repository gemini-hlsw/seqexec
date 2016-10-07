package edu.gemini.seqexec.engine

import scalaz._

/**
 * Flag to indicate whether the global execution is `Running` or `Waiting`.
 */
sealed trait Status

object Status {
  case object Running extends Status
  case object Waiting extends Status
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
    QueueZ.currentify(pending).map(QStateZ(_, status))

  val current: Current = Current.empty

  val done: Queue[Result] = Queue(Nil)

  def mark(i: Int)(r: Result): QState = self

}

case class QStateZ(zipper: QueueZ, status: Status) extends QState { self =>

  val next: Option[QState] = zipper.next match {
    // Last execution
    case None => zipper.uncurrentify.map(QStateF(_, status))
    case Some(x) => Some(QStateZ(x, status))
  }

  val current: Current = zipper.focus.focus.focus

  val pending: Queue[Action] = zipper.pending

  val done: Queue[Result] = zipper.done

  def mark(i: Int)(r: Result): QState = {

    val zipper: QStateZ @> QueueZ =
      Lens.lensu((qs, z) => qs.copy(zipper = z), _.zipper)

    val current: QStateZ @> Current = zipper >=> QueueZ.current

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
