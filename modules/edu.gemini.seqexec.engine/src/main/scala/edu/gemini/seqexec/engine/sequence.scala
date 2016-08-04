package edu.gemini.seqexec.engine

import scalaz.concurrent.Task

object Sequence {

  /**
    * A List of `Step`s meant to be run sequentially.
    */
  type Sequence = List[Step]

  /**
    *  A list of actions to be run in parallel
    */
  type Step = List[Action]

  type Action = Task[Result]

  /**
    * The result of an action.
    *
    */
  sealed trait Result
  case object OK extends Result
  case object Error extends Result

}
