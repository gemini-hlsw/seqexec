package edu.gemini.experimental.jluhrs

import scala.concurrent.duration.Duration
/**
  * Created by jluhrs on 5/19/16.
  */

// The source of time (can we really control time ?)
object Timer {
  type Time = Duration

  type Timer = Stream[Time]

  def periodicTimer(t0: Time, p: Time): Timer = t0 #:: periodicTimer(t0+p, p)
}


