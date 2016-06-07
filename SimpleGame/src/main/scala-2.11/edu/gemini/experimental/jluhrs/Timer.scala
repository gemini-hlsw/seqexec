package edu.gemini.experimental.jluhrs

/**
  * Created by jluhrs on 5/19/16.
  */

// The source of time (can we really control time ?)
object Timer {
  // For the sake of simplicity, Time is defined as Double
  type Time = Double

  type Timer = Stream[Time]

  def periodicTimer(t0: Time, p: Time): Timer = t0 #:: periodicTimer(t0+p, p)
}


