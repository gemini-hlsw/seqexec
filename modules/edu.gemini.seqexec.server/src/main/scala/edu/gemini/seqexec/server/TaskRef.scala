package edu.gemini.seqexec.server

import scalaz.concurrent.Task

/** A concurrent mutable cell for type A. */
sealed trait TaskRef[A] {

  /** Atomic modification. */
  def modify(f: A => A): Task[Unit]
  
  /** Return the current value. */
  def get: Task[A]

  /** Replace the current value. */
  def put(a: A): Task[Unit] = 
    modify(_ => a)

}

object TaskRef {

  /** Create a new TaskRef. */
  def newTaskRef[A](a: A): Task[TaskRef[A]] =
    Task.delay {
      @volatile var value = a
      new TaskRef[A] { ref =>
        def get = Task.delay(a)
        def modify(f: A => A) = Task.delay(ref.synchronized(value = f(a)))
      }
    }

}