package edu.gemini.experimental.jluhrs

/**
  * Created by jluhrs on 6/2/16.
  */

// My attempt to describe external inputs as a stream of values with a timestamp.

object EventStream {
  import Timer._

  type Event[A] = (Time, A)
  type EventStream[A] = Stream[Option[Event[A]]]

  // Returns all the events in the stream that happened before a given time.
  def pastEvents[A](es: EventStream[A], t: Time): (List[Event[A]], EventStream[A]) = {
    def f (s: EventStream[A], acc: List[Event[A]]): (List[Event[A]], EventStream[A]) = s match {
      case Some((t1, a)) #:: rest =>  if (t1 > t) (acc, s)
                                      else f (rest, (t1, a) :: acc)
      case _                      => (acc, s)
    }
    f(es, List()) match {
      case (l, s) => (l.reverse, s)
    }
  }
}
