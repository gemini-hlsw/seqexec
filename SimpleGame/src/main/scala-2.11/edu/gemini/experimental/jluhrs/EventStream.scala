package edu.gemini.experimental.jluhrs

import scalaz.concurrent.Task
import scalaz.stream.Process

/**
  * Created by jluhrs on 6/2/16.
  */

// My attempt to describe external inputs as a stream of values with a timestamp.

object EventStream {
  import Timer._

  type Event[A] = (Time, A)
  type EventStream[A] = Process[Task, Event[A]]

  // Returns all the events in the stream that happened before a given time.
//  def pastEvents[A](es: EventStream[A], t: Time): Process[Task, List[Event[A]]] =
//    es takeWhile  { case (t1, _) => t >= t1 } toList
}
