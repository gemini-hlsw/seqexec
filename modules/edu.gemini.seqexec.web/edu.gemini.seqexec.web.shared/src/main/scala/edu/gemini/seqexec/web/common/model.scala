package edu.gemini.seqexec.web.common

import scalaz._
import Scalaz._

/**
  * Minimal models to be exchanged between client and server
  */

case class StepConfig(key: String, value: String)
case class Step(id: Int, config: List[StepConfig])
case class SequenceSteps(steps: List[Step])
case class Sequence(id: String, steps: SequenceSteps)
